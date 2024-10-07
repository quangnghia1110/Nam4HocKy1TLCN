package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.address.DistrictEntity;
import studentConsulting.model.entity.address.WardEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.address.ManageWardDTO;
import studentConsulting.model.payload.request.address.WardRequest;
import studentConsulting.repository.address.DistrictRepository;
import studentConsulting.repository.address.WardRepository;
import studentConsulting.service.interfaces.admin.IAdminWardService;
import studentConsulting.specification.address.WardSpecification;

import java.util.Optional;

@Service
public class AdminWardServiceImpl implements IAdminWardService {

    @Autowired
    private WardRepository wardRepository;

    @Autowired
    private DistrictRepository districtRepository;

    private ManageWardDTO mapToDTO(WardEntity ward) {
        return ManageWardDTO.builder()
                .code(ward.getCode())
                .name(ward.getName())
                .nameEn(ward.getNameEn())
                .fullName(ward.getFullName())
                .fullNameEn(ward.getFullNameEn())
                .codeName(ward.getCodeName())
                .districtCode(ward.getDistrict() != null ? ward.getDistrict().getCode() : null)
                .build();
    }

    private WardEntity mapToEntity(WardRequest wardRequest, String code, String districtCode) {
        DistrictEntity district = districtRepository.findById(districtCode)
                .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã: " + districtCode));

        return WardEntity.builder()
                .code(code)
                .name(wardRequest.getName())
                .nameEn(wardRequest.getNameEn())
                .fullName(wardRequest.getFullName())
                .fullNameEn(wardRequest.getFullNameEn())
                .codeName(wardRequest.getCodeName())
                .district(district)
                .build();
    }

    @Override
    @Transactional
    public ManageWardDTO createWard(String code, String districtCode, WardRequest wardRequest) {
        WardEntity ward = mapToEntity(wardRequest, code, districtCode);
        WardEntity savedWard = wardRepository.save(ward);
        return mapToDTO(savedWard);
    }

    @Override
    @Transactional
    public ManageWardDTO updateWard(String code, String districtCode, WardRequest wardRequest) {
        WardEntity existingWard = wardRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Phường/Xã không tồn tại"));

        existingWard.setName(Optional.ofNullable(wardRequest.getName()).orElse(existingWard.getName()));
        existingWard.setNameEn(Optional.ofNullable(wardRequest.getNameEn()).orElse(existingWard.getNameEn()));
        existingWard.setFullName(Optional.ofNullable(wardRequest.getFullName()).orElse(existingWard.getFullName()));
        existingWard.setFullNameEn(Optional.ofNullable(wardRequest.getFullNameEn()).orElse(existingWard.getFullNameEn()));
        existingWard.setCodeName(Optional.ofNullable(wardRequest.getCodeName()).orElse(existingWard.getCodeName()));

        if (districtCode != null) {
            DistrictEntity district = districtRepository.findById(districtCode)
                    .orElseThrow(() -> new ErrorException("Không tìm thấy quận/huyện với mã: " + districtCode));
            existingWard.setDistrict(district);
        }

        WardEntity updatedWard = wardRepository.save(existingWard);
        return mapToDTO(updatedWard);
    }

    @Override
    @Transactional
    public void deleteWardByCode(String code) {
        WardEntity ward = wardRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Phường/Xã không tồn tại"));
        wardRepository.delete(ward);
    }

    @Override
    public ManageWardDTO getWardByCode(String code) {
        return wardRepository.findById(code)
                .map(this::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy Phường/Xã"));
    }

    @Override
    public Page<ManageWardDTO> getAllWardsWithFilters(Optional<String> code, Optional<String> name, Optional<String> nameEn, Optional<String> fullName, Optional<String> fullNameEn, Optional<String> codeName, Optional<String> districtCode, Pageable pageable) {
        Specification<WardEntity> spec = Specification.where(null);

        if (code.isPresent()) {
            spec = spec.and(WardSpecification.hasCode(code.get()));
        }

        if (name.isPresent()) {
            spec = spec.and(WardSpecification.hasName(name.get()));
        }

        if (nameEn.isPresent()) {
            spec = spec.and(WardSpecification.hasNameEn(nameEn.get()));
        }

        if (fullName.isPresent()) {
            spec = spec.and(WardSpecification.hasFullName(fullName.get()));
        }

        if (fullNameEn.isPresent()) {
            spec = spec.and(WardSpecification.hasFullNameEn(fullNameEn.get()));
        }

        if (codeName.isPresent()) {
            spec = spec.and(WardSpecification.hasCodeName(codeName.get()));
        }

        if (districtCode.isPresent()) {
            spec = spec.and(WardSpecification.hasDistrictCode(districtCode.get()));
        }

        return wardRepository.findAll(spec, pageable).map(this::mapToDTO);
    }

    @Override
    public boolean existsByCode(String code) {
        return wardRepository.existsByCode(code);
    }
}
