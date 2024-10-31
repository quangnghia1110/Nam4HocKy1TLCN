package studentConsulting.model.payload.mapper.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.repository.communication.ConversationUserRepository;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class ConversationMapper {

    private final ConversationUserRepository conversationUserRepository;

    @Autowired
    public ConversationMapper(ConversationUserRepository conversationUserRepository) {
        this.conversationUserRepository = conversationUserRepository;
    }

    public ConversationDTO mapToDTO(ConversationEntity conversation) {
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
                .map(member -> new MemberDTO(
                        member.getUser().getId(),
                        member.getUser().getLastName() + " " + member.getUser().getFirstName(),
                        member.getUser().getAvatarUrl(),
                        member.getUser().getAccount().getEmail().equals(currentUserEmail)
                ))
                .collect(Collectors.toList());

        dto.setMembers(members);

        return dto;
    }
}
