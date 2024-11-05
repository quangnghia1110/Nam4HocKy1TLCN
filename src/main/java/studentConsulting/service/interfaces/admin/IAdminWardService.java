package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageWardDTO;
import studentConsulting.model.payload.request.WardRequest;

public interface IAdminWardService {
    ManageWardDTO createWard(String code, String districtCode, WardRequest wardRequest);

    ManageWardDTO updateWard(String code, String districtCode, WardRequest wardRequest);

    void deleteWardByCode(String code);

    ManageWardDTO getWardByCode(String code);

    Page<ManageWardDTO> getWardByAdmin(String code, String name, String nameEn, String fullName, String fullNameEn, String codeName, String districtCode, Pageable pageable);

    boolean existsByCode(String code);
}
