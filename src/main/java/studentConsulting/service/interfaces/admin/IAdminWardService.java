package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.request.address.WardRequest;

import java.util.Optional;

public interface IAdminWardService {
    ManageWardDTO createWard(String code, String districtCode, WardRequest wardRequest);

    ManageWardDTO updateWard(String code, String districtCode, WardRequest wardRequest);

    void deleteWardByCode(String code);

    ManageWardDTO getWardByCode(String code);

    Page<ManageWardDTO> getAllWardsWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Optional<String> districtCode, Pageable pageable);

    boolean existsByCode(String code);
}
