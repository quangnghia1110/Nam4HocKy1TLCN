package studentConsulting.model.payload.request.authentication;

import lombok.Data;

@Data
public class RoleAskRequest {
    private String name;
    private Integer roleId;
}
