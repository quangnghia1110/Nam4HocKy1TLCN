package studentConsulting.service;

import java.util.List;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.FieldDTO;

public interface IDepartmentService {
    List<DepartmentDTO> getAllDepartments();
    List<FieldDTO> getFieldsByDepartment(Integer departmentId);
}
