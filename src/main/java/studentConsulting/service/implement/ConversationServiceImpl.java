package studentConsulting.service.implement;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.request.socket.ConversationRequest;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConversationService;

@Service
public class ConversationServiceImpl implements IConversationService {

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Override
    public ConversationDTO createConversation(ConversationRequest request, UserInformationEntity user) {
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

        boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), "ROLE_TUVANVIEN");
        
        if (!hasConsultantRole) {
            errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        ConversationEntity conversation = new ConversationEntity();
        conversation.setCreatedAt(LocalDateTime.now());
        conversation.setUser(user);
        conversation.setConsultant(consultant);
        conversation.setName(request.getName());
        conversation.setIsGroup(false);
        conversation.setStatusActive(true);
        conversation.setDepartment(department);

        ConversationEntity savedConversation = conversationRepository.save(conversation);

        return mapToDTO(savedConversation, consultant);
    }

    @Override
    public List<ConversationDTO> findConversationsByUserId(Integer userId) {
        List<ConversationEntity> conversations = conversationRepository.findAll();

        List<ConversationEntity> filteredConversations = conversations.stream()
                .filter(c -> c.getUser().getId().equals(userId) || c.getConsultant().getId().equals(userId))
                .collect(Collectors.toList());

        return filteredConversations.stream()
                .map(c -> mapToDTO(c, c.getConsultant()))
                .collect(Collectors.toList());
    }

    @Override
    public ConversationDTO findConversationById(Integer conversationId) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

        if (!conversationOpt.isPresent()) {
            throw new ResourceNotFoundException("Cuộc trò chuyện", "ID", conversationId);
        }

        ConversationEntity conversation = conversationOpt.get();
        
        return mapToDTO(conversation);
    }

    private ConversationDTO mapToDTO(ConversationEntity conversation) {
        return ConversationDTO.builder()
                .id(conversation.getId())
                .userName(conversation.getUser().getLastName() + " " + conversation.getUser().getFirstName())
                .consultantName(conversation.getConsultant().getLastName() + " " + conversation.getConsultant().getFirstName())
                .departmentId(conversation.getDepartment().getId())
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .build();
    }

    private ConversationDTO mapToDTO(ConversationEntity conversation, UserInformationEntity consultant) {
        return ConversationDTO.builder()
                .id(conversation.getId())
                .departmentId(conversation.getDepartment().getId())
                .userName(conversation.getUser().getLastName() + " " + conversation.getUser().getFirstName())
                .consultantName(consultant.getLastName() + " " + consultant.getFirstName())
                .name(conversation.getName())
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .build();
    }
}
