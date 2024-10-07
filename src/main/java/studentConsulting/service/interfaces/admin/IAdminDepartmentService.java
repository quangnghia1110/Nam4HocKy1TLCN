package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.department_field.ManageDepartmentDTO;
import studentConsulting.model.payload.request.department_field.DepartmentRequest;

import java.util.Optional;

public interface IAdminDepartmentService {
    ManageDepartmentDTO createDepartment(DepartmentRequest departmentRequest);

    ManageDepartmentDTO updateDepartment(Integer id, DepartmentRequest departmentRequest);

    void deleteDepartmentById(Integer id);

    ManageDepartmentDTO getDepartmentById(Integer id);

    Page<ManageDepartmentDTO> getAllDepartmentsWithFilters(Optional<String> name, Pageable pageable);

    boolean existsById(Integer id);
}
