package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.AddressEntity;
import studentConsulting.model.entity.DistrictEntity;
import studentConsulting.model.entity.ProvinceEntity;
import studentConsulting.model.entity.WardEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageAddressDTO;
import studentConsulting.model.payload.mapper.admin.AddressMapper;
import studentConsulting.model.payload.request.AddressRequest;
import studentConsulting.repository.admin.AddressRepository;
import studentConsulting.repository.admin.DistrictRepository;
import studentConsulting.repository.admin.ProvinceRepository;
import studentConsulting.repository.admin.WardRepository;
import studentConsulting.service.interfaces.admin.IAdminAdressService;
import studentConsulting.specification.admin.AddressSpecification;

import java.util.Optional;

@Service
public class AdminAddressServiceImpl implements IAdminAdressService {

    @Autowired
    private AddressRepository addressRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private WardRepository wardRepository;

    @Autowired
    private AddressMapper addressMapper;

    @Override
    public ManageAddressDTO createAddress(AddressRequest addressRequest) {
        ProvinceEntity province = provinceRepository.findById(addressRequest.getProvinceCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + addressRequest.getProvinceCode()));

        DistrictEntity district = districtRepository.findById(addressRequest.getDistrictCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy huyện với mã: " + addressRequest.getDistrictCode()));

        WardEntity ward = wardRepository.findById(addressRequest.getWardCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy xã với mã: " + addressRequest.getWardCode()));

        AddressEntity address = AddressEntity.builder()
                .line(addressRequest.getLine())
                .province(province)
                .district(district)
                .ward(ward)
                .build();

        AddressEntity savedAddress = addressRepository.save(address);

        return addressMapper.mapToDTO(savedAddress);
    }


    @Override
    @Transactional
    public ManageAddressDTO updateAddress(Integer id, AddressRequest addressRequest) {
        AddressEntity existingAddress = addressRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Địa chỉ không tồn tại"));

        existingAddress.setLine(Optional.ofNullable(addressRequest.getLine()).orElse(existingAddress.getLine()));

        if (addressRequest.getProvinceCode() != null) {
            ProvinceEntity province = provinceRepository.findById(addressRequest.getProvinceCode())
                    .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + addressRequest.getProvinceCode()));
            existingAddress.setProvince(province);
        }

        if (addressRequest.getDistrictCode() != null) {
            DistrictEntity district = districtRepository.findById(addressRequest.getDistrictCode())
                    .orElseThrow(() -> new ErrorException("Không tìm thấy huyện với mã: " + addressRequest.getDistrictCode()));
            existingAddress.setDistrict(district);
        }

        if (addressRequest.getWardCode() != null) {
            WardEntity ward = wardRepository.findById(addressRequest.getWardCode())
                    .orElseThrow(() -> new ErrorException("Không tìm thấy xã với mã: " + addressRequest.getWardCode()));
            existingAddress.setWard(ward);
        }

        AddressEntity updatedAddress = addressRepository.save(existingAddress);
        return addressMapper.mapToDTO(updatedAddress);
    }

    @Override
    @Transactional
    public void deleteAddressById(Integer id) {
        AddressEntity address = addressRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Địa chỉ không tồn tại"));
        addressRepository.delete(address);
    }

    @Override
    public ManageAddressDTO getAddressById(Integer id) {
        return addressRepository.findById(id)
                .map(addressMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy địa chỉ"));
    }

    @Override
    public Page<ManageAddressDTO> getAddressByAdmin(Integer id, String line, String provinceCode, String districtCode, String wardCode, Pageable pageable) {
        Specification<AddressEntity> spec = Specification.where(null);

        if (id != null) {
            spec = spec.and(AddressSpecification.hasId(id));
        }

        if (line != null && !line.isEmpty()) {
            spec = spec.and(AddressSpecification.hasLine(line));
        }

        if (provinceCode != null && !provinceCode.isEmpty()) {
            spec = spec.and(AddressSpecification.hasProvince(provinceCode));
        }

        if (districtCode != null && !districtCode.isEmpty()) {
            spec = spec.and(AddressSpecification.hasDistrict(districtCode));
        }

        if (wardCode != null && !wardCode.isEmpty()) {
            spec = spec.and(AddressSpecification.hasWard(wardCode));
        }

        return addressRepository.findAll(spec, pageable)
                .map(addressMapper::mapToDTO);
    }
}
