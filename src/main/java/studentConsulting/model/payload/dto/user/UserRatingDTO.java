package studentConsulting.model.payload.dto.user;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class UserRatingDTO {
    private Integer id;
    private String firstName;
    private String lastName;
    private String email;

}

