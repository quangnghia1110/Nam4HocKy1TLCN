package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.department_field.FieldDTO;

import java.util.List;

public interface ICommonDepartmentService {
    List<DepartmentDTO> getAllDepartments();

    List<FieldDTO> getFieldsByDepartment(Integer departmentId);
}
