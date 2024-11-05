package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.DistrictEntity;
import studentConsulting.model.entity.ProvinceEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageDistrictDTO;
import studentConsulting.model.payload.mapper.admin.DistrictMapper;
import studentConsulting.model.payload.request.DistrictRequest;
import studentConsulting.repository.admin.DistrictRepository;
import studentConsulting.repository.admin.ProvinceRepository;
import studentConsulting.service.interfaces.admin.IAdminDistrictService;
import studentConsulting.specification.admin.DistrictSpecification;

import java.util.Optional;

@Service
public class AdminDistrictServiceImpl implements IAdminDistrictService {

    @Autowired
    private DistrictRepository districtRepository;

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private DistrictMapper districtMapper;

    @Transactional
    public ManageDistrictDTO createDistrict(String code, String provinceCode, DistrictRequest districtRequest) {
        ProvinceEntity province = provinceRepository.findById(provinceCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + provinceCode));

        DistrictEntity district = DistrictEntity.builder()
                .code(code)
                .name(districtRequest.getName())
                .nameEn(districtRequest.getNameEn())
                .fullName(districtRequest.getFullName())
                .fullNameEn(districtRequest.getFullNameEn())
                .codeName(districtRequest.getCodeName())
                .province(province)
                .build();

        DistrictEntity savedDistrict = districtRepository.save(district);

        return districtMapper.mapToDTO(savedDistrict);
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
        return districtMapper.mapToDTO(updatedDistrict);
    }

    @Transactional
    public void deleteDistrictByCode(String code) {
        DistrictEntity district = districtRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Quận/Huyện không tồn tại"));
        districtRepository.delete(district);
    }

    public ManageDistrictDTO getDistrictByCode(String code) {
        return districtRepository.findById(code)
                .map(districtMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Quận/Huyện"));
    }

    public Page<ManageDistrictDTO> getDistrictByAdmin(String code, String name, String nameEn, String fullName,
                                                      String fullNameEn, String codeName, String provinceCode, Pageable pageable) {
        Specification<DistrictEntity> spec = Specification.where(null);

        if (code != null && !code.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasCode(code));
        }

        if (name != null && !name.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasName(name));
        }

        if (nameEn != null && !nameEn.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasNameEn(nameEn));
        }

        if (fullName != null && !fullName.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasFullName(fullName));
        }

        if (fullNameEn != null && !fullNameEn.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasFullNameEn(fullNameEn));
        }

        if (codeName != null && !codeName.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasCodeName(codeName));
        }

        if (provinceCode != null && !provinceCode.isEmpty()) {
            spec = spec.and(DistrictSpecification.hasProvinceCode(provinceCode));
        }

        return districtRepository.findAll(spec, pageable)
                .map(districtMapper::mapToDTO);
    }


    public boolean existsByCode(String code) {
        return districtRepository.existsByCode(code);
    }
}
