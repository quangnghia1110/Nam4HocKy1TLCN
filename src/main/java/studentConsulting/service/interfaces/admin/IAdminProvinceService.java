package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageProvinceDTO;
import studentConsulting.model.payload.request.ProvinceRequest;

public interface IAdminProvinceService {

    ManageProvinceDTO createProvince(ProvinceRequest provinceRequest, String code);

    ManageProvinceDTO updateProvince(String code, ProvinceRequest provinceRequest);

    void deleteProvinceByCode(String code);

    ManageProvinceDTO getProvinceByCode(String code);

    Page<ManageProvinceDTO> getProvinceByAdmin(String code, String name, String nameEn, String fullName, String fullNameEn, String codeName, Pageable pageable);

    boolean existsByCode(String code);
}
