package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.ProvinceEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageProvinceDTO;
import studentConsulting.model.payload.mapper.admin.ProvinceMapper;
import studentConsulting.model.payload.request.ProvinceRequest;
import studentConsulting.repository.admin.ProvinceRepository;
import studentConsulting.service.interfaces.admin.IAdminProvinceService;
import studentConsulting.specification.admin.ProvinceSpecification;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminProvinceServiceImpl implements IAdminProvinceService {

    @Autowired
    private ProvinceRepository provinceRepository;

    @Autowired
    private ProvinceMapper provinceMapper;

    @Override
    @Transactional
    public ManageProvinceDTO createProvince(ProvinceRequest provinceRequest, String code) {
        if (provinceRepository.existsByCode(code)) {
            throw new ErrorException("Mã tỉnh đã tồn tại: " + code);
        }

        ProvinceEntity province = ProvinceEntity.builder()
                .code(code)
                .name(provinceRequest.getName())
                .nameEn(provinceRequest.getNameEn())
                .fullName(provinceRequest.getFullName())
                .fullNameEn(provinceRequest.getFullNameEn())
                .codeName(provinceRequest.getCodeName())
                .build();

        ProvinceEntity savedProvince = provinceRepository.save(province);

        return provinceMapper.mapToDTO(savedProvince);
    }


    @Override
    @Transactional
    public ManageProvinceDTO updateProvince(String code, ProvinceRequest provinceRequest) {
        ProvinceEntity existingProvince = provinceRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));

        existingProvince.setName(Optional.ofNullable(provinceRequest.getName()).orElse(existingProvince.getName()));
        existingProvince.setNameEn(Optional.ofNullable(provinceRequest.getNameEn()).orElse(existingProvince.getNameEn()));
        existingProvince.setFullName(Optional.ofNullable(provinceRequest.getFullName()).orElse(existingProvince.getFullName()));
        existingProvince.setFullNameEn(Optional.ofNullable(provinceRequest.getFullNameEn()).orElse(existingProvince.getFullNameEn()));
        existingProvince.setCodeName(Optional.ofNullable(provinceRequest.getCodeName()).orElse(existingProvince.getCodeName()));

        ProvinceEntity updatedProvince = provinceRepository.save(existingProvince);
        return provinceMapper.mapToDTO(updatedProvince);
    }

    @Override
    @Transactional
    public void deleteProvinceByCode(String code) {
        ProvinceEntity province = provinceRepository.findById(code)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));
        provinceRepository.delete(province);
    }

    @Override
    public ManageProvinceDTO getProvinceByCode(String code) {
        return provinceRepository.findById(code)
                .map(provinceMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tỉnh với mã: " + code));
    }

    @Override
    public Page<ManageProvinceDTO> getAllProvincesWithFilters(String code, String name, String nameEn, String fullName,
                                                              String fullNameEn, String codeName, Pageable pageable) {
        Specification<ProvinceEntity> spec = Specification.where(null);

        if (code != null && !code.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasCode(code));
        }

        if (name != null && !name.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasName(name));
        }

        if (nameEn != null && !nameEn.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasNameEn(nameEn));
        }

        if (fullName != null && !fullName.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasFullName(fullName));
        }

        if (fullNameEn != null && !fullNameEn.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasFullNameEn(fullNameEn));
        }

        if (codeName != null && !codeName.isEmpty()) {
            spec = spec.and(ProvinceSpecification.hasCodeName(codeName));
        }

        return provinceRepository.findAll(spec, pageable)
                .map(provinceMapper::mapToDTO);
    }


    @Override
    public boolean existsByCode(String code) {
        return provinceRepository.existsByCode(code);
    }

    @Override
    public void importProvinces(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        List<ManageProvinceDTO> provinces = filteredData.stream()
                .map(row -> {
                    try {
                        String code = row.get(0);
                        String name = row.get(1);
                        String nameEn = row.get(2);
                        String fullName = row.get(3);
                        String fullNameEn = row.get(4);
                        String codeName = row.get(5);

                        return new ManageProvinceDTO(code, name, nameEn, fullName, fullNameEn, codeName);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Province: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        provinces.forEach(province -> {
            try {
                ProvinceEntity entity = new ProvinceEntity();
                entity.setCode(province.getCode());
                entity.setName(province.getName());
                entity.setNameEn(province.getNameEn());
                entity.setFullName(province.getFullName());
                entity.setFullNameEn(province.getFullNameEn());
                entity.setCodeName(province.getCodeName());

                provinceRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Province vào database: " + e.getMessage());
            }
        });
    }

}
