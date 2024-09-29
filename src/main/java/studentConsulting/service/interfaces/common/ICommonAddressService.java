package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.payload.dto.address.AddressDTO;
import studentConsulting.model.payload.dto.address.DistrictDTO;
import studentConsulting.model.payload.dto.address.ProvinceDTO;
import studentConsulting.model.payload.dto.address.WardDTO;

import java.util.List;

public interface ICommonAddressService {
    void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO);

    List<ProvinceDTO> getAllProvinces();

    List<DistrictDTO> getDistrictsByProvince(String provinceCode);

    List<WardDTO> getWardsByDistrict(String districtCode);
}
