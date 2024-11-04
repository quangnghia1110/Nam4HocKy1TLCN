package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageDistrictDTO;
import studentConsulting.model.payload.request.DistrictRequest;

import java.util.List;

public interface IAdminDistrictService {

    ManageDistrictDTO createDistrict(String code, String provinceCode, DistrictRequest districtRequest);

    ManageDistrictDTO updateDistrict(String code, String provinceCode, DistrictRequest districtRequest);

    void deleteDistrictByCode(String code);

    ManageDistrictDTO getDistrictByCode(String code);

    Page<ManageDistrictDTO> getAllDistrictsWithFilters(String code, String name, String nameEn, String fullName, String fullNameEn, String codeName, String provinceCode, Pageable pageable);

    boolean existsByCode(String code);

    void importDistricts(List<List<String>> csvData);

}
