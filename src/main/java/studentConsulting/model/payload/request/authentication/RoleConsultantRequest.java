package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class RoleConsultantRequest {
    private String name;
    private Integer roleId;
}

