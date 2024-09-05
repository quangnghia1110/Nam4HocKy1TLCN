package studentConsulting.service.implement;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.departmentField.FieldEntity;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.FieldDTO;
import studentConsulting.model.payload.dto.WardDTO;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.FieldRepository;
import studentConsulting.service.IDepartmentService;

@Service
public class DepartmentServiceImpl implements IDepartmentService{
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
}
