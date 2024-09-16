package studentConsulting.service.implement;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.UserDTO;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultantService;
import studentConsulting.specification.ConsultantSpecification;
import studentConsulting.specification.QuestionSpecification;

@Service
public class ConsultantServiceImpl implements IConsultantService {

    @Autowired
    private UserRepository userRepository;

    @Override
    public Page<ConsultantDTO> getFilteredConsultants(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<UserInformationEntity> spec = Specification.where(ConsultantSpecification.hasRole("TUVANVIEN"));

        if (departmentId != null) {
            spec = spec.and(ConsultantSpecification.hasDepartment(departmentId));
        }

        if (name != null && !name.trim().isEmpty()) {
            spec = spec.and(ConsultantSpecification.hasName(name.trim()));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultantSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultantSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultantSpecification.hasDateBefore(endDate));
        }

        return userRepository.findAll(spec, pageable).map(this::mapToConsultantDTO);
    }

    
    
    @Override
    public List<UserDTO> getConsultantsByDepartment(Integer departmentId) {
        List<UserInformationEntity> consultants = userRepository.findAll().stream()
            .filter(user -> user.getAccount().getRole().getName().equals("TUVANVIEN") &&
                            user.getAccount().getDepartment().getId().equals(departmentId))
            .collect(Collectors.toList());
        
        return consultants.stream()
                .map(consultant -> new UserDTO(consultant.getId(), consultant.getFirstName(), consultant.getLastName()))
                .collect(Collectors.toList());
    }
    
    private ConsultantDTO mapToConsultantDTO(UserInformationEntity userInfo) {
        return ConsultantDTO.builder()
                .id(userInfo.getAccount().getId())
                .firstName(userInfo.getFirstName())
                .lastName(userInfo.getLastName())
                .email(userInfo.getAccount().getEmail())
                .phone(userInfo.getPhone())
                .avatarUrl(userInfo.getAvatarUrl())
                .departmentId(userInfo.getAccount().getDepartment() != null ? userInfo.getAccount().getDepartment().getId() : null)
                .build();
    }
}



