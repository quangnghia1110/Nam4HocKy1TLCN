package studentConsulting.service.implement.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKeyEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.mapper.ConversationMapper;
import studentConsulting.model.payload.request.socket.CreateConversationUserRequest;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.user.IUserConversationService;
import studentConsulting.specification.communication.ConversationSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserConversationServiceImpl implements IUserConversationService {

    @Autowired
    private ConversationMapper conversationMapper;

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConversationUserRepository conversationUserRepository;

    @Autowired
    private MessageRepository messageRepository;

    @Override
    public ConversationDTO createConversation(CreateConversationUserRequest request, UserInformationEntity user) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
        if (!consultantOpt.isPresent()) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không tồn tại"));
        }

        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(request.getDepartmentId());
        if (!departmentOpt.isPresent()) {
            errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        UserInformationEntity consultant = consultantOpt.get();
        DepartmentEntity department = departmentOpt.get();

        if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc phòng ban đã chọn"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        boolean conversationExists = conversationRepository.existsByUserAndConsultantAndDepartment(user, consultant, department);

        if (conversationExists) {
            throw new ErrorException("Cuộc trò chuyện giữa bạn và tư vấn viên này trong phòng ban đã tồn tại");
        }

        ConversationEntity conversation = new ConversationEntity();
        conversation.setCreatedAt(LocalDate.now());
        conversation.setUser(user);
        conversation.setConsultant(consultant);
        conversation.setIsGroup(false);
        conversation.setStatusActive(true);
        conversation.setDepartment(department);

        ConversationEntity savedConversation = conversationRepository.save(conversation);

        ConversationUserEntity conversationUserForCreator = new ConversationUserEntity();
        ConversationUserKeyEntity creatorKey = new ConversationUserKeyEntity(savedConversation.getId(), user.getId());
        conversationUserForCreator.setId(creatorKey);
        conversationUserForCreator.setConversation(savedConversation);
        conversationUserForCreator.setUser(user);
        conversationUserRepository.save(conversationUserForCreator);

        ConversationUserEntity conversationUserForConsultant = new ConversationUserEntity();
        ConversationUserKeyEntity consultantKey = new ConversationUserKeyEntity(savedConversation.getId(), consultant.getId());
        conversationUserForConsultant.setId(consultantKey);
        conversationUserForConsultant.setConversation(savedConversation);
        conversationUserForConsultant.setUser(consultant);
        conversationUserRepository.save(conversationUserForConsultant);

        return conversationMapper.mapToDTO(savedConversation, consultant);
    }

    @Override
    public Page<ConversationDTO> getListConversationByRole(Integer userId, String role, Integer depId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<ConversationEntity> spec;

        if (SecurityConstants.Role.USER.equals(role)) {
            spec = Specification.where(ConversationSpecification.isOwner(userId))
                    .or(ConversationSpecification.isMember(userId));
        } else if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            spec = Specification.where(ConversationSpecification.isOwner(userId))
                    .or(ConversationSpecification.isMember(userId));
        } else if (SecurityConstants.Role.ADMIN.equals(role)) {
            spec = Specification.where(null);
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            spec = Specification.where(ConversationSpecification.hasDepartment(depId));
        }
        else {
            throw new ErrorException("Vai trò không được hỗ trợ");
        }

        if (name != null && !name.trim().isEmpty()) {
            spec = spec.and(ConversationSpecification.hasName(name));
        }
        if (startDate != null && endDate != null) {
            spec = spec.and(ConversationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConversationSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConversationSpecification.hasDateBefore(endDate));
        }

        Page<ConversationEntity> conversations = conversationRepository.findAll(spec, pageable);

        return conversations.map(conversationMapper::mapToDTO);
    }

    @Override
    public ConversationDTO getDetailConversationByRole(Integer conversationId) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Nhóm không tồn tại");
        }

        ConversationEntity conversation = conversationOpt.get();

        return conversationMapper.mapToDTO(conversation);
    }
}
