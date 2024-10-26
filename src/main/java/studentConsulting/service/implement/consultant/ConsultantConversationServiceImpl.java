package studentConsulting.service.implement.consultant;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.authentication.AccountEntity;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.communication.ConversationUserEntity;
import studentConsulting.model.entity.communication.ConversationUserKeyEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.user.EmailDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.model.payload.mapper.ConversationMapper;
import studentConsulting.model.payload.request.socket.CreateConversationRequest;
import studentConsulting.repository.authentication.AccountRepository;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.consultant.IConsultantConversationService;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class ConsultantConversationServiceImpl implements IConsultantConversationService {

    @Autowired
    private ConversationMapper conversationMapper;

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

        return conversationMapper.mapToDTO(savedConversation, user);
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
    public ConversationDTO approveMembersByEmail(Integer groupId, List<String> emailsToApprove) {
        ConversationEntity group = conversationRepository.findById(groupId)
                .orElseThrow(() -> new ErrorException("Nhóm không tồn tại"));

        for (String emailToApprove : emailsToApprove) {
            UserInformationEntity user = userRepository.findUserInfoByEmail(emailToApprove)
                    .orElseThrow(() -> new ErrorException("Người dùng với email này không tồn tại"));

            boolean isMember = conversationUserRepository.existsByConversationAndUser(group, user);

            if (!isMember) {
                ConversationUserKeyEntity key = new ConversationUserKeyEntity(group.getId(), user.getId());

                ConversationUserEntity conversationUser = new ConversationUserEntity();
                conversationUser.setId(key);
                conversationUser.setConversation(group);
                conversationUser.setUser(user);

                conversationUserRepository.save(conversationUser);
            }
        }

        conversationRepository.save(group);

        return conversationMapper.mapToDTO(group);
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
