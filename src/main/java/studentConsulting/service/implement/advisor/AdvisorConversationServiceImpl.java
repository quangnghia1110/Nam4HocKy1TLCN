package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.repository.communication.ConversationRepository;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.repository.communication.MessageRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConversationService;
import studentConsulting.specification.communication.ConversationSpecification;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdvisorConversationServiceImpl implements IAdvisorConversationService {

    @Autowired
    private ConversationRepository conversationRepository;

    @Autowired
    private ConversationUserRepository conversationUserRepository;

    @Autowired
    private MessageRepository messageRepository;

    @Autowired
    private UserRepository userRepository;

    @Override
    @Transactional
    public void deleteConversation(Integer conversationId, Integer departmentId) {
        Optional<ConversationEntity> conversationOpt;

        if (departmentId == null) { // Admin không cần kiểm tra phòng ban
            conversationOpt = conversationRepository.findById(conversationId);
        } else {
            conversationOpt = conversationRepository.findByIdAndDepartmentId(conversationId, departmentId);
        }

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại hoặc không thuộc phòng ban của bạn");
        }

        ConversationEntity conversation = conversationOpt.get();
        messageRepository.deleteMessagesByConversationId(conversationId);
        conversationUserRepository.deleteMembersByConversation(conversation);
        conversationRepository.delete(conversation);
    }

    @Override
    @Transactional
    public void updateConversationName(Integer conversationId, String newName, Integer departmentId) {
        Optional<ConversationEntity> conversationOpt;

        if (departmentId == null) {
            conversationOpt = conversationRepository.findById(conversationId);
        } else {
            conversationOpt = conversationRepository.findByIdAndDepartmentId(conversationId, departmentId);
        }

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại hoặc không thuộc phòng ban của bạn");
        }

        ConversationEntity conversation = conversationOpt.get();
        conversation.setName(newName);
        conversationRepository.save(conversation);
    }

    @Override
    @Transactional
    public void removeMemberFromConversation(Integer conversationId, Integer userId, Integer departmentId) {
        Optional<ConversationEntity> conversationOpt;
        if (departmentId == null) {
            conversationOpt = conversationRepository.findById(conversationId);
        } else {
            conversationOpt = conversationRepository.findByIdAndDepartmentId(conversationId, departmentId);
        }

        Optional<UserInformationEntity> userOpt = userRepository.findById(userId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại hoặc không thuộc phòng ban của bạn");
        }

        if (!userOpt.isPresent()) {
            throw new ErrorException("Người dùng không tồn tại");
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
    public Page<ConversationDTO> findConversationsByDepartmentWithFilters(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<ConversationEntity> spec = Specification.where(departmentId != null
                ? ConversationSpecification.hasDepartment(departmentId)
                : null);

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

    @Override
    public ConversationDTO getConversationByIdAndDepartment(Integer conversationId, Integer departmentId) {
        Optional<ConversationEntity> conversationOpt = departmentId == null
                ? conversationRepository.findById(conversationId)
                : conversationRepository.findByIdAndDepartmentId(conversationId, departmentId);

        if (!conversationOpt.isPresent()) {
            throw new ErrorException("Cuộc trò chuyện không tồn tại");
        }

        return mapToDTO(conversationOpt.get());
    }

}
