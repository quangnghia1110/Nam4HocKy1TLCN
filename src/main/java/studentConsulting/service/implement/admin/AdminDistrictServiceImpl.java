package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.ProvinceEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageDistrictDTO;
import studentConsulting.model.payload.request.address.DistrictRequest;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.ProvinceRepository;
import studentConsulting.service.interfaces.admin.IAdminDistrictService;
import studentConsulting.specification.address.DistrictSpecification;

import java.util.Optional;

@Service
public class AdminDistrictServiceImpl implements IAdminDistrictService {

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    private ManageDistrictDTO mapToDTO(DistrictEntity district) {
        return ManageDistrictDTO.builder()
                .code(district.getCode())
                .name(district.getName())
                .nameEn(district.getNameEn())
                .fullName(district.getFullName())
                .fullNameEn(district.getFullNameEn())
                .codeName(district.getCodeName())
                .provinceCode(district.getProvince() != null ? district.getProvince().getCode() : null)
                .build();
    }

    private DistrictEntity mapToEntity(DistrictRequest districtRequest, String code, String provinceCode) {
        ProvinceEntity province = provinceRepository.findById(provinceCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + provinceCode));

        return DistrictEntity.builder()
                .code(code)
                .name(districtRequest.getName())
                .nameEn(districtRequest.getNameEn())
                .fullName(districtRequest.getFullName())
                .fullNameEn(districtRequest.getFullNameEn())
                .codeName(districtRequest.getCodeName())
                .province(province)
                .build();
    }

    @Transactional
    public ManageDistrictDTO createDistrict(String code, String provinceCode, DistrictRequest districtRequest) {
        DistrictEntity district = mapToEntity(districtRequest, code, provinceCode);
        DistrictEntity savedDistrict = districtRepository.save(district);
        return mapToDTO(savedDistrict);
    }

    @Transactional
    public ManageDistrictDTO updateDistrict(String code, String provinceCode, DistrictRequest districtRequest) {
        DistrictEntity existingDistrict = districtRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Quận/Huyện không tồn tại"));

        existingDistrict.setName(Optional.ofNullable(districtRequest.getName()).orElse(existingDistrict.getName()));
        existingDistrict.setNameEn(Optional.ofNullable(districtRequest.getNameEn()).orElse(existingDistrict.getNameEn()));
        existingDistrict.setFullName(Optional.ofNullable(districtRequest.getFullName()).orElse(existingDistrict.getFullName()));
        existingDistrict.setFullNameEn(Optional.ofNullable(districtRequest.getFullNameEn()).orElse(existingDistrict.getFullNameEn()));
        existingDistrict.setCodeName(Optional.ofNullable(districtRequest.getCodeName()).orElse(existingDistrict.getCodeName()));

        if (provinceCode != null) {
            ProvinceEntity province = provinceRepository.findById(provinceCode)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + provinceCode));
            existingDistrict.setProvince(province);
        }

        DistrictEntity updatedDistrict = districtRepository.save(existingDistrict);
        return mapToDTO(updatedDistrict);
    }

    @Transactional
    public void deleteDistrictByCode(String code) {
        DistrictEntity district = districtRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Quận/Huyện không tồn tại"));
        districtRepository.delete(district);
    }

    public ManageDistrictDTO getDistrictByCode(String code) {
        return districtRepository.findById(code)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Quận/Huyện"));
    }

    public Page<ManageDistrictDTO> getAllDistrictsWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Optional<String> provinceCode, Pageable pageable) {
        Specification<DistrictEntity> spec = Specification.where(null);

        if (code.isPresent()) {
            spec = spec.and(DistrictSpecification.hasCode(code.get()));
        }

        if (name.isPresent()) {
            spec = spec.and(DistrictSpecification.hasName(name.get()));
        }

        if (nameEn.isPresent()) {
            spec = spec.and(DistrictSpecification.hasNameEn(nameEn.get()));
        }

        if (fullName.isPresent()) {
            spec = spec.and(DistrictSpecification.hasFullName(fullName.get()));
        }

        if (fullNameEn.isPresent()) {
            spec = spec.and(DistrictSpecification.hasFullNameEn(fullNameEn.get()));
        }

        if (codeName.isPresent()) {
            spec = spec.and(DistrictSpecification.hasCodeName(codeName.get()));
        }

        if (provinceCode.isPresent()) {
            spec = spec.and(DistrictSpecification.hasProvinceCode(provinceCode.get()));
        }

        return districtRepository.findAll(spec, pageable)
                .map(this::mapToDTO);
    }

    public boolean existsByCode(String code) {
        return districtRepository.existsByCode(code);
    }
}
