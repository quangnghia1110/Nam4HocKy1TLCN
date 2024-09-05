package studentConsulting.service;

import java.util.List;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.AddressDTO;
import studentConsulting.model.payload.dto.DistrictDTO;
import studentConsulting.model.payload.dto.ProvinceDTO;
import studentConsulting.model.payload.dto.WardDTO;

public interface IAddressService {
	public void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO);

	public List<ProvinceDTO> getAllProvinces();

	public List<DistrictDTO> getDistrictsByProvince(String provinceCode);

	public List<WardDTO> getWardsByDistrict(String districtCode);
}
