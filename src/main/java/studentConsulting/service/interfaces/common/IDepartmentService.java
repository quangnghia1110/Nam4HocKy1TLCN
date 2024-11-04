package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.actor.DepartmentDTO;
import studentConsulting.model.payload.dto.actor.FieldDTO;

import java.util.List;

public interface IDepartmentService {
    List<DepartmentDTO> getAllDepartments();

    List<FieldDTO> getFieldsByDepartment(Integer departmentId);
}
