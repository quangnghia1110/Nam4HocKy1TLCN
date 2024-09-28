package studentConsulting.model.payload.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class CommentDTO {
    private String text;
    private UserDTO user;
    private LocalDate create_date;
}
