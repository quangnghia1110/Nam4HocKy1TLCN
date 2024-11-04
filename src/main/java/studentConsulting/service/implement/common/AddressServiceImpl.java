package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.AddressEntity;
import studentConsulting.model.entity.DistrictEntity;
import studentConsulting.model.entity.ProvinceEntity;
import studentConsulting.model.entity.WardEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.AddressDTO;
import studentConsulting.model.payload.dto.actor.DistrictDTO;
import studentConsulting.model.payload.dto.actor.ProvinceDTO;
import studentConsulting.model.payload.dto.actor.WardDTO;
import studentConsulting.repository.admin.AddressRepository;
import studentConsulting.repository.admin.DistrictRepository;
import studentConsulting.repository.admin.ProvinceRepository;
import studentConsulting.repository.admin.WardRepository;
import studentConsulting.service.interfaces.common.IAddressService;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class AddressServiceImpl implements IAddressService {

    @Autowired
    private AddressRepository addressRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private WardRepository wardRepository;

    public void updateAddress(UserInformationEntity userEntity, AddressDTO addressDTO) {
        ProvinceEntity provinceEntity = provinceRepository.findById(addressDTO.getProvinceCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh/thành phố với mã đã cung cấp."));

        DistrictEntity districtEntity = districtRepository.findById(addressDTO.getDistrictCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã đã cung cấp."));

        if (!districtEntity.getProvince().getCode().equals(provinceEntity.getCode())) {
            throw new ErrorException("Quận/huyện không thuộc tỉnh/thành phố đã chọn.");
        }

        WardEntity wardEntity = wardRepository.findById(addressDTO.getWardCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy phường/xã với mã đã cung cấp."));

        if (!wardEntity.getDistrict().getCode().equals(districtEntity.getCode())) {
            throw new ErrorException("Phường/xã không thuộc quận/huyện đã chọn.");
        }

        AddressEntity addressEntity = userEntity.getAddress();
        if (addressEntity == null) {
            addressEntity = new AddressEntity();
        }
        addressEntity.setLine(addressDTO.getLine());
        addressEntity.setProvince(provinceEntity);
        addressEntity.setDistrict(districtEntity);
        addressEntity.setWard(wardEntity);

        userEntity.setAddress(addressEntity);
        addressRepository.save(addressEntity);
    }


    @Override
    public List<ProvinceDTO> getAllProvinces() {
        return provinceRepository.findAll().stream()
                .map(province -> new ProvinceDTO(province.getCode(), province.getFullName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<DistrictDTO> getDistrictsByProvince(String provinceCode) {
        List<DistrictEntity> districts = districtRepository.findAll().stream()
                .filter(district -> district.getProvince().getCode().equals(provinceCode))
                .collect(Collectors.toList());

        return districts.stream()
                .map(district -> new DistrictDTO(district.getCode(), district.getFullName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<WardDTO> getWardsByDistrict(String districtCode) {
        List<WardEntity> wards = wardRepository.findAll().stream()
                .filter(ward -> ward.getDistrict().getCode().equals(districtCode))
                .collect(Collectors.toList());

        return wards.stream()
                .map(ward -> new WardDTO(ward.getCode(), ward.getFullName()))
                .collect(Collectors.toList());
    }
}
