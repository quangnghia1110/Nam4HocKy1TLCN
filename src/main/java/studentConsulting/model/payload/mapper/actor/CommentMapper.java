package studentConsulting.model.payload.mapper.actor;

import org.springframework.stereotype.Component;
import studentConsulting.model.entity.content.CommentEntity;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.dto.user.UserDTO;

@Component
public class CommentMapper {

    public CommentDTO mapToDTO(CommentEntity comment) {
        UserDTO userDTO = new UserDTO();
        userDTO.setId(comment.getUserComment().getId());
        userDTO.setFirstName(comment.getUserComment().getFirstName());
        userDTO.setLastName(comment.getUserComment().getLastName());

        CommentDTO dto = new CommentDTO();
        dto.setText(comment.getComment());
        dto.setUser(userDTO);
        dto.setCreate_date(comment.getCreateDate());
        dto.setPostId(comment.getPost().getId());

        return dto;
    }
}
