package studentConsulting.model.payload.mapper.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import studentConsulting.model.entity.communication.ConversationEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.communication.ConversationDTO;
import studentConsulting.model.payload.dto.user.MemberDTO;
import studentConsulting.repository.communication.ConversationUserRepository;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class ConversationMapper {

    private final ConversationUserRepository conversationUserRepository;

    @Autowired
    public ConversationMapper(ConversationUserRepository conversationUserRepository) {
        this.conversationUserRepository = conversationUserRepository;
    }

    public ConversationDTO mapToDTO(ConversationEntity conversation, UserInformationEntity currentUser) {
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

    public ConversationDTO mapToDTO(ConversationEntity conversation) {
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
                        false // Giả định rằng phương thức này không quan tâm đến vai trò của người dùng hiện tại
                ))
                .collect(Collectors.toList());

        dto.setMembers(members);

        return dto;
    }
}
