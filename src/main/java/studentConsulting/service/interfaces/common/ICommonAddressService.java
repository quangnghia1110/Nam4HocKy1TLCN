package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.AddressDTO;
import studentConsulting.model.payload.dto.DistrictDTO;
import studentConsulting.model.payload.dto.ProvinceDTO;
import studentConsulting.model.payload.dto.WardDTO;

import java.util.List;

public interface ICommonAddressService {
    void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO);

    List<ProvinceDTO> getAllProvinces();

    List<DistrictDTO> getDistrictsByProvince(String provinceCode);

    List<WardDTO> getWardsByDistrict(String districtCode);
}
