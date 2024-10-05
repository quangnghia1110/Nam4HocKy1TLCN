package studentConsulting.service.interfaces.common;

import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.department_field.FieldDTO;
import studentConsulting.model.payload.dto.department_field.ImportDepartmentDTO;
import studentConsulting.model.payload.dto.department_field.ImportFieldDTO;

import java.util.List;

public interface ICommonDepartmentService {
    List<DepartmentDTO> getAllDepartments();

    List<FieldDTO> getFieldsByDepartment(Integer departmentId);

    void importDepartments(List<List<String>> csvData);

    void importFields(List<List<String>> csvData);
    
    void saveImportedDepartments(List<ImportDepartmentDTO> departments);

    void saveImportedFields(List<ImportFieldDTO> fields);
}
