package studentConsulting.model.payload.dto.content;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import studentConsulting.model.payload.dto.user.UserDTO;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CommentDTO {
    private String text;
    private UserDTO user;
    private LocalDate create_date;
}
