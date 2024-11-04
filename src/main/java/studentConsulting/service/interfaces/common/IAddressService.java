package studentConsulting.service.interfaces.common;

import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.AddressDTO;
import studentConsulting.model.payload.dto.actor.DistrictDTO;
import studentConsulting.model.payload.dto.actor.ProvinceDTO;
import studentConsulting.model.payload.dto.actor.WardDTO;

import java.util.List;

public interface IAddressService {
    void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO);

    List<ProvinceDTO> getAllProvinces();

    List<DistrictDTO> getDistrictsByProvince(String provinceCode);

    List<WardDTO> getWardsByDistrict(String districtCode);
}
