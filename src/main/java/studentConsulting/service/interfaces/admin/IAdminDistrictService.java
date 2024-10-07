package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;
import studentConsulting.model.payload.request.address.DistrictRequest;

import java.util.Optional;

public interface IAdminDistrictService {

    ManageDistrictDTO createDistrict(String code, String provinceCode, DistrictRequest districtRequest);

    ManageDistrictDTO updateDistrict(String code, String provinceCode, DistrictRequest districtRequest);

    void deleteDistrictByCode(String code);

    ManageDistrictDTO getDistrictByCode(String code);

    Page<ManageDistrictDTO> getAllDistrictsWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Optional<String> provinceCode, Pageable pageable);

    boolean existsByCode(String code);

}
