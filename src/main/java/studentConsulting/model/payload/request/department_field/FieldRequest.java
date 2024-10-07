package studentConsulting.model.payload.request.department_field;

import lombok.Data;

@Data
public class FieldRequest {
    private String name;
    private Integer departmentId;
}
