package studentConsulting.service.implement.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKeyEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.user.EmailDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantConversationService;
import studentConsulting.specification.communication.ConversationSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class ConsultantConversationServiceImpl implements IConsultantConversationService {

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConversationUserRepository conversationUserRepository;

    @Autowired
    private MessageRepository messageRepository;

    @Override
    public ConversationDTO createConversationByConsultant(CreateConversationRequest request,
                                                          UserInformationEntity user) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        boolean hasConsultantRole = userRepository.existsByUserIdAndRoleName(user.getId(), SecurityConstants.Role.TUVANVIEN);
        if (!hasConsultantRole) {
            errors.add(new FieldErrorDetail("role", "Người dùng không có vai trò tư vấn viên"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        ConversationEntity conversation = new ConversationEntity();
        conversation.setCreatedAt(LocalDate.now());
        conversation.setConsultant(user);
        conversation.setUser(user);
        conversation.setName(request.getName());
        conversation.setIsGroup(true);
        conversation.setStatusActive(true);
        conversation.setDepartment(user.getAccount().getDepartment());

        ConversationEntity savedConversation = conversationRepository.save(conversation);

        ConversationUserKeyEntity conversationUserKey = new ConversationUserKeyEntity(savedConversation.getId(), user.getId());
        ConversationUserEntity conversationUser = new ConversationUserEntity();
        conversationUser.setId(conversationUserKey);
        conversationUser.setConversation(savedConversation);
        conversationUser.setUser(user);

        conversationUserRepository.save(conversationUser);

        return mapToDTO(savedConversation, user);
    }

    @Override
    public Page<ConversationDTO> findConversationsByConsultantWithFilters(Integer consultantId, String name,
                                                                          LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConversationEntity> spec = Specification
                .where(ConversationSpecification.hasConsultant(consultantId))
                .and(ConversationSpecification.hasRoleConsultant());

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
    @Transactional
    public ConversationDTO approveMembersByEmail(Integer groupId, List<String> emailsToApprove) {
        // Tìm nhóm theo ID
        ConversationEntity group = conversationRepository.findById(groupId)
                .orElseThrow(() -> new ErrorException("Nhóm không tồn tại"));

        // Duyệt qua từng email để thêm thành viên vào nhóm
        for (String emailToApprove : emailsToApprove) {
            // Tìm người dùng theo email
            UserInformationEntity user = userRepository.findUserInfoByEmail(emailToApprove)
                    .orElseThrow(() -> new ErrorException("Người dùng với email này không tồn tại"));

            // Kiểm tra xem người dùng đã là thành viên chưa
            boolean isMember = conversationUserRepository.existsByConversationAndUser(group, user);

            if (!isMember) {
                // Tạo khóa cho ConversationUserKeyEntity
                ConversationUserKeyEntity key = new ConversationUserKeyEntity(group.getId(), user.getId());

                // Tạo mới ConversationUserEntity và lưu vào cơ sở dữ liệu
                ConversationUserEntity conversationUser = new ConversationUserEntity();
                conversationUser.setId(key);
                conversationUser.setConversation(group);
                conversationUser.setUser(user);

                conversationUserRepository.save(conversationUser);
            }
        }

        // Lưu nhóm sau khi cập nhật danh sách thành viên
        conversationRepository.save(group);

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
        } else {
            throw new ErrorException("Người dùng không phải là thành viên của cuộc trò chuyện này");
        }
    }

    @Override
    public List<MemberDTO> findNonConsultantMembers(Integer conversationId) {
        UserDetails userDetails = (UserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        Integer currentUserId = accountRepository.findByEmail(userDetails.getUsername())
                                .map(AccountEntity::getId)
                                .orElseThrow(() -> new ErrorException("Người dùng không được tìm thấy"));


        List<ConversationUserEntity> members = conversationUserRepository.findByConversationId(conversationId);

        return members.stream()
                .filter(member -> !userRepository.existsByUserIdAndRoleName(member.getUser().getId(), SecurityConstants.Role.TUVANVIEN))
                .map(member -> new MemberDTO(
                        member.getUser().getId(),
                        member.getUser().getLastName() + " " + member.getUser().getFirstName(),
                        member.getUser().getAvatarUrl(),
                        member.getUser().getId().equals(currentUserId)
                ))
                .sorted((m1, m2) -> Boolean.compare(m2.isSender(), m1.isSender()))
                .collect(Collectors.toList());
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
    public List<EmailDTO> findAllUsersWithRoleUser() {
        List<UserInformationEntity> users = userRepository.findAllByRole(SecurityConstants.Role.USER);

        return users.stream()
                .map(user -> new EmailDTO(
                        user.getId(),
                        user.getAccount().getEmail(),
                        user.getLastName() + " " + user.getFirstName(),
                        user.getAvatarUrl()
                ))
                .collect(Collectors.toList());
    }

}
