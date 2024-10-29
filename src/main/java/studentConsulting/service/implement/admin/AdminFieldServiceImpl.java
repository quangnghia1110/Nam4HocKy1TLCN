package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.department_field.ImportFieldDTO;
import studentConsulting.model.payload.dto.department_field.ManageFieldDTO;
import studentConsulting.model.payload.mapper.admin.FieldMapper;
import studentConsulting.model.payload.request.department_field.FieldRequest;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.service.interfaces.admin.IAdminFieldService;
import studentConsulting.specification.department_field.FieldSpecification;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdminFieldServiceImpl implements IAdminFieldService {

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldMapper fieldMapper;

    @Override
    public ManageFieldDTO createField(Integer departmentId, FieldRequest fieldRequest) {
        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + departmentId));

        FieldEntity field = FieldEntity.builder()
                .name(fieldRequest.getName())
                .department(department)
                .createdAt(LocalDate.now())
                .build();

        FieldEntity savedField = fieldRepository.save(field);

        return fieldMapper.mapToDTO(savedField);
    }


    @Override
    public ManageFieldDTO updateField(Integer id, Integer departmentId, FieldRequest fieldRequest) {
        FieldEntity existingField = fieldRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));

        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + departmentId));

        existingField.setName(fieldRequest.getName());
        existingField.setDepartment(department);
        FieldEntity updatedField = fieldRepository.save(existingField);
        return fieldMapper.mapToDTO(updatedField);
    }


    @Override
    @Transactional
    public void deleteFieldById(Integer id) {
        FieldEntity field = fieldRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));
        fieldRepository.delete(field);
    }

    @Override
    public ManageFieldDTO getFieldById(Integer id) {
        return fieldRepository.findById(id)
                .map(fieldMapper::mapToDTO)
                .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + id));
    }

    public Page<ManageFieldDTO> getAllFieldsWithFilters(String name, String departmentId, Pageable pageable) {
        Specification<FieldEntity> spec = Specification.where(null);

        if (name != null && !name.isEmpty()) {
            spec = spec.and(FieldSpecification.hasName(name));
        }

        if (departmentId != null && !departmentId.isEmpty()) {
            spec = spec.and(FieldSpecification.hasDepartmentId(departmentId));
        }

        return fieldRepository.findAll(spec, pageable)
                .map(fieldMapper::mapToDTO);
    }

    @Override
    public boolean existsById(Integer id) {
        return fieldRepository.existsById(id);
    }

    @Override
    public void importFields(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ImportFieldDTO> fields = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        LocalDate createdAt = LocalDate.parse(row.get(1), formatter);
                        String name = row.get(2);
                        Integer departmentId = Integer.parseInt(row.get(3));

                        return new ImportFieldDTO(id, createdAt, name, departmentId);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Field");
                    }
                })
                .collect(Collectors.toList());

        fields.forEach(field -> {
            try {
                FieldEntity entity = new FieldEntity();
                entity.setId(field.getId());
                entity.setCreatedAt(field.getCreatedAt());
                entity.setName(field.getName());

                DepartmentEntity department = departmentRepository.findById(field.getDepartmentId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Department ID không hợp lệ"));

                entity.setDepartment(department);

                fieldRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Field vào database");
            }
        });
    }

    @Override
    public String getDepartmentNameById(Integer departmentId) {
        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(departmentId);
        if (departmentOpt.isPresent()) {
            return departmentOpt.get().getName();
        } else {
            throw new IllegalArgumentException("Không tìm thấy phòng ban với ID: " + departmentId);
        }
    }

}
