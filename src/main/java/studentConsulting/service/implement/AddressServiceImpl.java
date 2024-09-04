package studentConsulting.service.implement;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.AddressDTO;
import studentConsulting.repository.AddressRepository;
import studentConsulting.repository.DistrictRepository;
import studentConsulting.repository.ProvinceRepository;
import studentConsulting.repository.WardRepository;
import studentConsulting.service.IAddressService;

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
            .orElseThrow(() -> new RuntimeException("Không tìm thấy tỉnh/thành phố với mã đã cung cấp."));

        DistrictEntity districtEntity = districtRepository.findById(addressDTO.getDistrictCode())
            .orElseThrow(() -> new RuntimeException("Không tìm thấy quận/huyện với mã đã cung cấp."));
        
        if (!districtEntity.getProvince().getCode().equals(provinceEntity.getCode())) {
            throw new RuntimeException("Quận/huyện không thuộc tỉnh/thành phố đã chọn.");
        }

        WardEntity wardEntity = wardRepository.findById(addressDTO.getWardCode())
            .orElseThrow(() -> new RuntimeException("Không tìm thấy phường/xã với mã đã cung cấp."));
        
        if (!wardEntity.getDistrict().getCode().equals(districtEntity.getCode())) {
            throw new RuntimeException("Phường/xã không thuộc quận/huyện đã chọn.");
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
}
