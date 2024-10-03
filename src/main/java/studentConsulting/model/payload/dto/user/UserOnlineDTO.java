package studentConsulting.model.payload.dto.user;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class UserOnlineDTO {
    private Integer id;
    private String fullName;
    private String email;
    private String phone;
    private String status;
    private String avatarUrl;
}
