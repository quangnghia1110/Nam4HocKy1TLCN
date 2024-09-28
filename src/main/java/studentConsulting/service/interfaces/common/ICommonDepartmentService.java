package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.FieldDTO;

import java.util.List;

public interface ICommonDepartmentService {
    List<DepartmentDTO> getAllDepartments();

    List<FieldDTO> getFieldsByDepartment(Integer departmentId);
}
