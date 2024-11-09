package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Context;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import studentConsulting.model.entity.ConversationEntity;
import studentConsulting.model.payload.dto.actor.ConversationDTO;
import studentConsulting.model.payload.dto.actor.MemberDTO;
import studentConsulting.repository.actor.ConversationUserRepository;

import java.util.List;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface ConversationMapper {

    @Mapping(source = "id", target = "id")
    @Mapping(source = "department.id", target = "department.id")
    @Mapping(source = "department.name", target = "department.name")
    @Mapping(source = "isGroup", target = "isGroup")
    @Mapping(source = "createdAt", target = "createdAt")
    @Mapping(source = "name", target = "name")
    @Mapping(source = "avatarUrl", target = "avatarUrl")
    @Mapping(target = "members", source = "conversation", qualifiedByName = "mapMembers")
    ConversationDTO mapToDTO(ConversationEntity conversation, @Context ConversationUserRepository conversationUserRepository);

    @Named("mapMembers")
    default List<MemberDTO> mapMembers(ConversationEntity conversation, @Context ConversationUserRepository conversationUserRepository) {
        UserDetails userDetails = (UserDetails) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        String currentUserEmail = userDetails.getUsername();

        return conversationUserRepository.findAll().stream()
                .filter(member -> member.getConversation().equals(conversation))
                .map(member -> new MemberDTO(
                        member.getUser().getId(),
                        member.getUser().getLastName() + " " + member.getUser().getFirstName(),
                        member.getUser().getAvatarUrl(),
                        member.getUser().getAccount().getEmail().equals(currentUserEmail)
                ))
                .collect(Collectors.toList());
    }
}
