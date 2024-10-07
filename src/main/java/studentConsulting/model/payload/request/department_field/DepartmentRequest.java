package studentConsulting.model.payload.request.department_field;

import lombok.Data;

@Data
public class DepartmentRequest {
    private String name;
    private String description;
    private String logo;
}
