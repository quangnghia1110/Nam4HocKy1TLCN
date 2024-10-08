package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.AddressEntity;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageAddressDTO;
import studentConsulting.model.payload.request.address.AddressRequest;
import studentConsulting.repository.address.AddressRepository;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.repository.address.WardRepository;
import studentConsulting.service.interfaces.admin.IAdminAdressService;
import studentConsulting.specification.address.AddressSpecification;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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

    @Override
    public ManageAddressDTO createAddress(AddressRequest addressRequest) {
        AddressEntity address = mapToEntity(addressRequest);
        AddressEntity savedAddress = addressRepository.save(address);
        return mapToDTO(savedAddress);
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
        return mapToDTO(updatedAddress);
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
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy địa chỉ"));
    }

    @Override
    public Page<ManageAddressDTO> getAllAddresses(Pageable pageable) {
        return addressRepository.findAll(pageable)
                .map(this::mapToDTO);
    }

    @Override
    public Page<ManageAddressDTO> getAllAddressesWithFilters(Integer id, String line, String provinceCode, String districtCode, String wardCode, Pageable pageable) {
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
                .map(this::mapToDTO);
    }


    private ManageAddressDTO mapToDTO(AddressEntity address) {
        return ManageAddressDTO.builder()
                .line(address.getLine())
                .provinceCode(address.getProvince() != null ? address.getProvince().getCode() : null)
                .districtCode(address.getDistrict() != null ? address.getDistrict().getCode() : null)
                .wardCode(address.getWard() != null ? address.getWard().getCode() : null)
                .build();
    }

    private AddressEntity mapToEntity(AddressRequest addressRequest) {
        ProvinceEntity province = provinceRepository.findById(addressRequest.getProvinceCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + addressRequest.getProvinceCode()));

        DistrictEntity district = districtRepository.findById(addressRequest.getDistrictCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy huyện với mã: " + addressRequest.getDistrictCode()));

        WardEntity ward = wardRepository.findById(addressRequest.getWardCode())
                .orElseThrow(() -> new ErrorException("Không tìm thấy xã với mã: " + addressRequest.getWardCode()));

        return AddressEntity.builder()
                .line(addressRequest.getLine())
                .province(province)
                .district(district)
                .ward(ward)
                .build();
    }

    @Override
    public void importAddresses(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageAddressDTO> addresses = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String line = row.get(1);
                        String provinceCode = row.get(2);
                        String districtCode = row.get(3);
                        String wardCode = row.get(4);

                        return new ManageAddressDTO(id, line, provinceCode, districtCode, wardCode);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Address: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        addresses.forEach(address -> {
            try {
                AddressEntity entity = new AddressEntity();
                entity.setId(address.getId());
                entity.setLine(address.getLine());

                ProvinceEntity province = provinceRepository.findByCode(address.getProvinceCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tỉnh với mã: " + address.getProvinceCode()));
                DistrictEntity district = districtRepository.findByCode(address.getDistrictCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy huyện với mã: " + address.getDistrictCode()));
                WardEntity ward = wardRepository.findByCode(address.getWardCode())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy xã với mã: " + address.getWardCode()));

                entity.setProvince(province);
                entity.setDistrict(district);
                entity.setWard(ward);

                addressRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Address vào database: " + e.getMessage());
            }
        });
    }


}
