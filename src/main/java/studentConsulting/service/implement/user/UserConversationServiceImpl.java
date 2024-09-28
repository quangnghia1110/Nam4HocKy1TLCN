package studentConsulting.service.implement.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKeyEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationUserRequest;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.ConversationUserRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.repository.common.DepartmentRepository;
import studentConsulting.repository.common.MessageRepository;
import studentConsulting.service.interfaces.user.IUserConversationService;
import studentConsulting.specification.ConversationSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class UserConversationServiceImpl implements IUserConversationService {

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

        // Tạo cuộc trò chuyện mới
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

        return mapToDTO(savedConversation, consultant);
    }

    @Override
    public Page<ConversationDTO> findConversationsByUserWithFilters(Integer userId, String name, LocalDate startDate,
                                                                    LocalDate endDate, Pageable pageable) {

        Specification<ConversationEntity> spec = Specification.where(ConversationSpecification.isOwner(userId))
                .or(ConversationSpecification.isMember(userId));

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

        return conversations.map(this::mapToDTO);
    }

    @Override
    public ConversationDTO findConversationById(Integer conversationId) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Nhóm không tồn tại");
        }

        ConversationEntity conversation = conversationOpt.get();

        return mapToDTO(conversation);
    }

    private ConversationDTO mapToDTO(ConversationEntity conversation) {
        UserDetails userDetails = (UserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        String currentUserEmail = userDetails.getUsername();

        ConversationDTO dto = ConversationDTO.builder()
                .id(conversation.getId())
                .department(conversation.getDepartment() != null
                        ? new DepartmentDTO(conversation.getDepartment().getId(), conversation.getDepartment().getName())
                        : null)
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .name(conversation.getName())
                .build();

        List<MemberDTO> members = conversationUserRepository.findAll().stream()
                .filter(member -> member.getConversation().equals(conversation))
                .map(member -> {
                    boolean isSender = member.getUser().getAccount().getEmail().equals(currentUserEmail);
                    return new MemberDTO(
                            member.getUser().getId(),
                            member.getUser().getLastName() + " " + member.getUser().getFirstName(),
                            member.getUser().getAvatarUrl(),
                            isSender
                    );
                })
                .collect(Collectors.toList());

        boolean isCurrentUserReceiver = members.stream()
                .anyMatch(member -> !member.isSender());

        if (isCurrentUserReceiver) {
            Collections.reverse(members);
        }

        dto.setMembers(members);

        return dto;
    }


    private ConversationDTO mapToDTO(ConversationEntity conversation, UserInformationEntity currentUser) {
        ConversationDTO dto = ConversationDTO.builder()
                .id(conversation.getId())
                .department(conversation.getDepartment() != null
                        ? new DepartmentDTO(conversation.getDepartment().getId(), conversation.getDepartment().getName())
                        : null)
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .name(conversation.getName())
                .build();

        List<MemberDTO> members = conversationUserRepository.findAll().stream()
                .filter(member -> member.getConversation().equals(conversation))
                .map(member -> new MemberDTO(
                        member.getUser().getId(),
                        member.getUser().getLastName() + " " + member.getUser().getFirstName(),
                        member.getUser().getAvatarUrl(),
                        member.getUser().getId().equals(currentUser.getId())
                ))
                .sorted((m1, m2) -> Boolean.compare(m2.isSender(), m1.isSender()))
                .collect(Collectors.toList());

        dto.setMembers(members);

        return dto;
    }


    @Override
    @Transactional
    public void deleteConversation(Integer conversationId) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        ConversationEntity conversation = conversationOpt.get();

        messageRepository.deleteMessagesByConversationId(conversationId);

        conversationUserRepository.deleteMembersByConversation(conversation);

        conversationRepository.delete(conversation);
    }
}
