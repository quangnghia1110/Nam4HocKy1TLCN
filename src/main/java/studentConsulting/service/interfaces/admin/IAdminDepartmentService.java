package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageDepartmentDTO;
import studentConsulting.model.payload.request.DepartmentRequest;

import java.util.List;

public interface IAdminDepartmentService {
    ManageDepartmentDTO createDepartment(DepartmentRequest departmentRequest);

    ManageDepartmentDTO updateDepartment(Integer id, DepartmentRequest departmentRequest);

    void deleteDepartmentById(Integer id);

    ManageDepartmentDTO getDepartmentById(Integer id);

    Page<ManageDepartmentDTO> getAllDepartmentsWithFilters(String name, Pageable pageable);

    boolean existsById(Integer id);

    void importDepartments(List<List<String>> csvData);

}
