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

        // Kiểm tra sự tồn tại của tư vấn viên
        Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
        if (!consultantOpt.isPresent()) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không tồn tại"));
        }

        // Kiểm tra sự tồn tại của phòng ban
        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(request.getDepartmentId());
        if (!departmentOpt.isPresent()) {
            errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
        }
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
        

        // Lấy thông tin từ consultant và department
        UserInformationEntity consultant = consultantOpt.get();
        DepartmentEntity department = departmentOpt.get();

        // Kiểm tra xem tư vấn viên có thuộc đơn vị tư vấn đã chọn hay không
        if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc phòng ban đã chọn"));
        }
        boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(consultant.getId(), "TUVANVIEN");
        
        // In ra kết quả kiểm tra vai trò
        System.out.println("Has consultant role: " + hasConsultantRole);

        if (!hasConsultantRole) {
            errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
        }
        
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        // Tạo ConversationEntity mới
        ConversationEntity conversation = new ConversationEntity();
        conversation.setCreatedAt(LocalDateTime.now());
        conversation.setUser(user);
        conversation.setConsultant(consultant);
        conversation.setName(request.getName());
        conversation.setIsGroup(false);
        conversation.setStatusActive(true);
        conversation.setDepartment(department);

        // Lưu cuộc trò chuyện vào cơ sở dữ liệu
        ConversationEntity savedConversation = conversationRepository.save(conversation);

        // Trả về DTO của cuộc trò chuyện
        return mapToDTO(savedConversation, consultant);
    }
   
    @Override
    public List<ConversationDTO> findConversationsByUserId(Integer userId) {
        List<ConversationEntity> conversations = conversationRepository.findAll();
        // Lọc những cuộc trò chuyện có userId là người dùng hoặc tư vấn viên
        List<ConversationEntity> filteredConversations = conversations.stream()
                .filter(c -> c.getUser().getId().equals(userId) || c.getConsultant().getId().equals(userId))
                .collect(Collectors.toList());

        // Chuyển đổi danh sách ConversationEntity thành danh sách ConversationDTO
        return filteredConversations.stream()
                .map(c -> mapToDTO(c, c.getConsultant()))
                .collect(Collectors.toList());
    }
    
    @Override
    public ConversationDTO findConversationById(Integer conversationId) {
        // Lấy thông tin cuộc trò chuyện theo ID
        List<FieldErrorDetail> errors = new ArrayList<>();

    	Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

    	if (!conversationOpt.isPresent()) {
            throw new ResourceNotFoundException("Cuộc trò chuyện", "ID", conversationId);
        }

        ConversationEntity conversation = conversationOpt.get();
        
        // Chuyển đổi entity sang DTO
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
    
    // Chuyển đổi từ ConversationEntity sang ConversationDTO
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
