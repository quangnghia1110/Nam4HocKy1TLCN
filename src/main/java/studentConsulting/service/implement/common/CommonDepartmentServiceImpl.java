package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.dto.department_field.FieldDTO;
import studentConsulting.model.payload.dto.department_field.ImportDepartmentDTO;
import studentConsulting.model.payload.dto.department_field.ImportFieldDTO;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.service.interfaces.common.ICommonDepartmentService;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class CommonDepartmentServiceImpl implements ICommonDepartmentService {
    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Override
    public List<DepartmentDTO> getAllDepartments() {
        return departmentRepository.findAll().stream()
                .map(department -> new DepartmentDTO(department.getId(), department.getName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<FieldDTO> getFieldsByDepartment(Integer departmentId) {
        List<FieldEntity> fields = fieldRepository.findAll().stream()
                .filter(field -> field.getDepartment().getId().equals(departmentId))
                .collect(Collectors.toList());

        return fields.stream()
                .map(field -> new FieldDTO(field.getId(), field.getName()))
                .collect(Collectors.toList());
    }

    @Override
    public void importDepartments(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)
                .collect(Collectors.toList());

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        List<ImportDepartmentDTO> departments = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        LocalDate createdAt = LocalDate.parse(row.get(1), formatter);  // Chuyển đổi thành LocalDate
                        String description = row.get(2);
                        String logo = row.get(3);
                        String name = row.get(4);

                        return new ImportDepartmentDTO(id, createdAt, description, logo, name);
                    } catch (Exception e) {
                        throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Department: ");
                    }
                })
                .collect(Collectors.toList());

        departments.forEach(department -> {
            try {
                DepartmentEntity entity = new DepartmentEntity();
                entity.setId(department.getId());
                entity.setCreatedAt(department.getCreatedAt());
                entity.setDescription(department.getDescription());
                entity.setLogo(department.getLogo());
                entity.setName(department.getName());

                departmentRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Department vào database: ");
            }
        });
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
    public void saveImportedDepartments(List<ImportDepartmentDTO> departments) {
        departments.forEach(department -> {
            DepartmentEntity entity = new DepartmentEntity();
            entity.setId(department.getId());
            entity.setCreatedAt(department.getCreatedAt());
            entity.setDescription(department.getDescription());
            entity.setLogo(department.getLogo());
            entity.setName(department.getName());

            departmentRepository.save(entity);
        });
    }

    @Override
    public void saveImportedFields(List<ImportFieldDTO> fields) {
        fields.forEach(field -> {
            FieldEntity entity = new FieldEntity();
            entity.setId(field.getId());
            entity.setCreatedAt(field.getCreatedAt());
            entity.setName(field.getName());

            DepartmentEntity department = departmentRepository.findById(field.getDepartmentId())
                    .orElseThrow(() -> new Exceptions.ErrorException("Department ID không hợp lệ"));

            entity.setDepartment(department);
            fieldRepository.save(entity);
        });
    }
}
