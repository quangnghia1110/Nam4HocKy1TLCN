package studentConsulting.model.payload.dto.authentication;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RoleDTO {
    private Integer id;
    private String name; 
}