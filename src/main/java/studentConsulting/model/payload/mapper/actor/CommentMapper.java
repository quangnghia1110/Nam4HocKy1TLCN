package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.content.CommentEntity;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.dto.user.UserDTO;

import java.util.ArrayList;
import java.util.List;

@Component
public class CommentMapper {

    public CommentDTO mapToDTO(CommentEntity comment) {
        return mapToDTOWithChildren(comment);
    }

    private CommentDTO mapToDTOWithChildren(CommentEntity comment) {
        UserDTO userDTO = new UserDTO();
        userDTO.setId(comment.getUserComment().getId());
        userDTO.setName(comment.getUserComment().getLastName() + " " + comment.getUserComment().getFirstName());
        userDTO.setAvatarUrl(comment.getUserComment().getAvatarUrl());

        CommentDTO dto = new CommentDTO();
        dto.setId(comment.getIdComment());
        dto.setParentCommentId(comment.getParentComment() != null ? comment.getParentComment().getIdComment() : null);
        dto.setText(comment.getComment());
        dto.setUser(userDTO);
        dto.setCreate_date(comment.getCreateDate());
        dto.setPostId(comment.getPost().getId());

        // Kiểm tra null trước khi ánh xạ childComments
        List<CommentDTO> childComments = new ArrayList<>();
        if (comment.getChildComments() != null) {
            for (CommentEntity childComment : comment.getChildComments()) {
                CommentDTO childDTO = mapToDTOWithChildren(childComment);
                childComments.add(childDTO);
            }
        }
        dto.setChildComments(childComments);

        return dto;
    }
}
