package studentConsulting.service.implement;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKey;
import studentConsulting.model.entity.communication.MessageEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.Exceptions.ResourceNotFoundException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConversationDTO;
import studentConsulting.model.payload.dto.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.repository.ConversationRepository;
import studentConsulting.repository.ConversationUserRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.MessageRepository;
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

    @Autowired
    private ConversationUserRepository conversationUserRepository;

    @Autowired
    private MessageRepository messageRepository;
    
    @Override
    public ConversationDTO createConversation(CreateConversationRequest request, UserInformationEntity user) {
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
    public ConversationDTO createConversationByConsultant(CreateConversationRequest request, UserInformationEntity user) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(request.getDepartmentId());
        if (!departmentOpt.isPresent()) {
            errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        DepartmentEntity department = departmentOpt.get();

        if (!user.getAccount().getDepartment().getId().equals(department.getId())) {
            errors.add(new FieldErrorDetail("consultant", "Người dùng không thuộc phòng ban đã chọn"));
        }

        boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(user.getId(), "ROLE_TUVANVIEN");
        if (!hasConsultantRole) {
            errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        ConversationEntity conversation = new ConversationEntity();
        conversation.setCreatedAt(LocalDateTime.now());
        conversation.setConsultant(user); 
        conversation.setUser(user);
        conversation.setName(request.getName());
        conversation.setIsGroup(true);
        conversation.setStatusActive(true);
        conversation.setDepartment(department);

        ConversationEntity savedConversation = conversationRepository.save(conversation);

        ConversationUserKey conversationUserKey = new ConversationUserKey(savedConversation.getId(), user.getId());
        ConversationUserEntity conversationUser = new ConversationUserEntity();
        conversationUser.setId(conversationUserKey);
        conversationUser.setConversation(savedConversation);
        conversationUser.setUser(user);

        conversationUserRepository.save(conversationUser);

        return mapToDTO(savedConversation, user);
    }


    @Override
    public List<ConversationDTO> findConversationsByUserId(Integer userId) {
        List<ConversationEntity> conversations = conversationRepository.findAll();

        List<ConversationEntity> filteredConversations = conversations.stream()
                .filter(c -> (c.getUser() != null && c.getUser().getId().equals(userId)) || 
                             (c.getConsultant() != null && c.getConsultant().getId().equals(userId)))
                .collect(Collectors.toList());

        return filteredConversations.stream()
                .map(c -> mapToDTO(c, c.getConsultant()))
                .collect(Collectors.toList());
    }
    
    @Override
    public List<ConversationDTO> findConversationsByConsultantId(Integer consultantId) {
        List<ConversationEntity> conversations = conversationRepository.findAll();

        List<ConversationEntity> filteredConversations = conversations.stream()
                .filter(c -> c.getConsultant() != null && c.getConsultant().getId().equals(consultantId))
                .collect(Collectors.toList());

        return filteredConversations.stream()
                .map(c -> mapToDTO(c, c.getConsultant()))
                .collect(Collectors.toList());
    }


    @Override
    @Transactional
    public ConversationDTO approveMember(Integer groupId, Integer userId) {
        ConversationEntity group = conversationRepository.findById(groupId)
            .orElseThrow(() -> new ErrorException("Nhóm không tồn tại"));

        UserInformationEntity user = userRepository.findById(userId)
            .orElseThrow(() -> new ErrorException("Người dùng không tồn tại"));

        boolean isMember = conversationUserRepository.existsByConversationAndUser(group, user);

        if (!isMember) {
            ConversationUserKey key = new ConversationUserKey(group.getId(), user.getId());
            
            ConversationUserEntity conversationUser = new ConversationUserEntity();
            conversationUser.setId(key);
            conversationUser.setConversation(group);
            conversationUser.setUser(user);
            
            conversationUserRepository.save(conversationUser);
            conversationRepository.save(group);
        }

        return mapToDTO(group);
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
        ConversationDTO dto = ConversationDTO.builder()
                .id(conversation.getId())
                .userName(conversation.getUser() != null 
                    ? conversation.getUser().getLastName() + " " + conversation.getUser().getFirstName() 
                    : "Người dùng không xác định") 
                .consultantName(conversation.getConsultant() != null 
                    ? conversation.getConsultant().getLastName() + " " + conversation.getConsultant().getFirstName() 
                    : "Tư vấn viên không xác định") 
                .departmentId(conversation.getDepartment().getId())
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .name(conversation.getName())
                .build();

        List<MemberDTO> members = conversationUserRepository.findAll().stream()
                .filter(member -> member.getConversation().equals(conversation))
                .map(member -> new MemberDTO(member.getUser().getId(), member.getUser().getLastName() + " " + member.getUser().getFirstName()))
                .collect(Collectors.toList());

        dto.setMembers(members);

        return dto;
    }



    private ConversationDTO mapToDTO(ConversationEntity conversation, UserInformationEntity consultant) {
        ConversationDTO dto = ConversationDTO.builder()
                .id(conversation.getId())
                .userName(conversation.getUser().getLastName() + " " + conversation.getUser().getFirstName())
                .consultantName(consultant.getLastName() + " " + consultant.getFirstName())
                .departmentId(conversation.getDepartment().getId())
                .isGroup(conversation.getIsGroup())
                .createdAt(conversation.getCreatedAt())
                .name(conversation.getName())
                .build();

        if (conversation.getMembers() != null) {
        	List<MemberDTO> members = conversationUserRepository.findAll().stream()
                    .filter(member -> member.getConversation().equals(conversation))
                    .map(member -> new MemberDTO(member.getUser().getId(), member.getUser().getLastName() + " " + member.getUser().getFirstName()))
                    .collect(Collectors.toList());

            dto.setMembers(members);; 
        }

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

        messageRepository.deleteMessagesByConversation(conversation);
        conversationUserRepository.deleteMembersByConversation(conversation);
        conversationRepository.deleteConversation(conversation);
    }


    @Override
    @Transactional
    public void updateConversationName(Integer conversationId, String newName) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        ConversationEntity conversation = conversationOpt.get();
        conversation.setName(newName); 
        conversationRepository.save(conversation);  
    }

    @Override
    @Transactional
    public void removeMemberFromConversation(Integer conversationId, Integer userId) {
        Optional<ConversationEntity> conversationOpt = conversationRepository.findById(conversationId);
        Optional<UserInformationEntity> userOpt = userRepository.findById(userId);

        if (!conversationOpt.isPresent() || !userOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện hoặc người dùng không tồn tại");
        }

        ConversationEntity conversation = conversationOpt.get();
        UserInformationEntity user = userOpt.get();

        boolean isMember = conversationUserRepository.existsByConversationAndUser(conversation, user);

        if (isMember) {
            conversationUserRepository.deleteByConversationAndUser(conversation, user);  
        }
    }
    
    public List<MemberDTO> findNonConsultantMembers(Integer conversationId) {
        List<ConversationUserEntity> members = conversationUserRepository.findByConversationId(conversationId);

        return members.stream()
            .filter(member -> !userRepository.existsByUserIdAndRoleName(member.getUser().getId(), "ROLE_TUVANVIEN"))
            .map(member -> new MemberDTO(member.getUser().getId(), member.getUser().getLastName() + " " + member.getUser().getFirstName()))
            .collect(Collectors.toList());
    }


}
