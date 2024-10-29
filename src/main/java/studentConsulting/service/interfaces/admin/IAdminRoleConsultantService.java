package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.user.ManageRoleConsultantDTO;
import studentConsulting.model.payload.request.authentication.RoleConsultantRequest;

import java.util.List;
import java.util.Optional;

public interface IAdminRoleConsultantService {

    ManageRoleConsultantDTO createRoleConsultant(Integer roleId, RoleConsultantRequest roleConsultantRequest);

    ManageRoleConsultantDTO updateRoleConsultant(Integer id, Integer roleId, RoleConsultantRequest roleConsultantRequest);

    void deleteRoleConsultantById(Integer id);

    ManageRoleConsultantDTO getRoleConsultantById(Integer id);

    Page<ManageRoleConsultantDTO> getAllRoleConsultantsWithFilters(String name, Optional<Integer> roleId, Pageable pageable);

    boolean existsById(Integer id);

    void importRoleConsultants(List<List<String>> csvData);
}

