package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageRoleAskDTO;
import studentConsulting.model.payload.request.RoleAskRequest;

import java.util.List;
import java.util.Optional;

public interface IAdminRoleAskService {

    ManageRoleAskDTO createRoleAsk(Integer roleId, RoleAskRequest roleAskRequest);

    ManageRoleAskDTO updateRoleAsk(Integer id, Integer roleId, RoleAskRequest roleAskRequest);

    void deleteRoleAskById(Integer id);

    ManageRoleAskDTO getRoleAskById(Integer id);

    Page<ManageRoleAskDTO> getAllRoleAsksWithFilters(String name, Optional<Integer> roleId, Pageable pageable);

    boolean existsById(Integer id);

    void importRoleAsks(List<List<String>> csvData);
}
