package studentConsulting.model.payload.mapper.actor;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.Named;
import studentConsulting.model.entity.CommentEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.CommentDTO;
import studentConsulting.model.payload.dto.actor.UserDTO;

import java.util.List;
import java.util.stream.Collectors;

@Mapper(componentModel = "spring")
public interface CommentMapper {

    // Ánh xạ từ CommentEntity sang CommentDTO
    @Mapping(source = "userComment", target = "user", qualifiedByName = "mapUserToUserDTO")
    @Mapping(source = "idComment", target = "id")
    @Mapping(source = "parentComment.idComment", target = "parentCommentId")
    @Mapping(source = "comment", target = "text")
    @Mapping(source = "createDate", target = "create_date")
    @Mapping(source = "post.id", target = "postId")
    @Mapping(source = "childComments", target = "childComments", qualifiedByName = "mapChildComments")
    CommentDTO mapToDTO(CommentEntity comment);

    @Named("mapUserToUserDTO")
    default UserDTO mapUserToUserDTO(UserInformationEntity user) {
        if (user == null) return null;

        UserDTO userDTO = new UserDTO();
        userDTO.setId(user.getId());
        userDTO.setLastName(user.getLastName());
        userDTO.setFirstName(" " + user.getFirstName());
        return userDTO;
    }

    @Named("mapChildComments")
    default List<CommentDTO> mapChildComments(List<CommentEntity> childComments) {
        return childComments != null ? childComments.stream()
                .map(this::mapToDTO)
                .collect(Collectors.toList()) : List.of();
    }
}
