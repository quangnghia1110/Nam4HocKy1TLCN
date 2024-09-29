package studentConsulting.model.payload.dto.user;

import lombok.Builder;
import lombok.Data;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;

@Data
@Builder 
public class ConsultantDTO {
	private Integer id;
    private String firstName;
    private String lastName;
    private String phone;
    private String email;
    private String avatarUrl;
    private DepartmentDTO department;
}
