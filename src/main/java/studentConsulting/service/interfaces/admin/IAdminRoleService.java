package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.authentication.RoleDTO;
import studentConsulting.model.payload.request.authentication.RoleRequest;

import java.util.Optional;

public interface IAdminRoleService {
    RoleDTO createRole(RoleRequest roleRequest);

    RoleDTO updateRole(Integer id, RoleRequest roleRequest);

    void deleteRoleById(Integer id);

    RoleDTO getRoleById(Integer id);

    Page<RoleDTO> getAllRolesWithFilters(Optional<String> name, Pageable pageable);

    boolean existsById(Integer id);
}
