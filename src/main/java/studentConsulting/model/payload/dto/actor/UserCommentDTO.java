package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserCommentDTO {
    private Integer id;
    private String firstName;
    private String lastName;
    private String avatarUrl;
}

