package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.address.ManageProvinceDTO;
import studentConsulting.model.payload.request.address.ProvinceRequest;

import java.util.List;
import java.util.Optional;

public interface IAdminProvinceService {

    ManageProvinceDTO createProvince(ProvinceRequest provinceRequest, String code);

    ManageProvinceDTO updateProvince(String code, ProvinceRequest provinceRequest);

    void deleteProvinceByCode(String code);

    ManageProvinceDTO getProvinceByCode(String code);

    Page<ManageProvinceDTO> getAllProvincesWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Pageable pageable);

    boolean existsByCode(String code);

    void importProvinces(List<List<String>> csvData);

}
