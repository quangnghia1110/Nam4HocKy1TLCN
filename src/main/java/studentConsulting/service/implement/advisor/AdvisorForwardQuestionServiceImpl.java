package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.question_answer.ForwardQuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.ForwardQuestionDTO;
import studentConsulting.repository.question_answer.ForwardQuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorForwardQuestionService;
import studentConsulting.specification.question_answer.ForwardQuestionSpecification;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdvisorForwardQuestionServiceImpl implements IAdvisorForwardQuestionService {

    @Autowired
    private ForwardQuestionRepository forwardQuestionRepository;
    @Autowired
    private UserRepository userRepository;

    @Override
    public Page<ForwardQuestionDTO> getForwardQuestionsWithFilters(Integer toDepartmentId, LocalDate startDate, LocalDate endDate, Pageable pageable, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Người dùng không tồn tại.");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();

        Specification<ForwardQuestionEntity> spec = Specification.where(ForwardQuestionSpecification.hasFromDepartment(departmentId));

        spec = spec.or(ForwardQuestionSpecification.hasToDepartment(departmentId));

        if (toDepartmentId != null) {
            spec = spec.and(ForwardQuestionSpecification.hasToDepartment(toDepartmentId));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ForwardQuestionSpecification.hasDateBefore(endDate));
        }

        Page<ForwardQuestionEntity> forwardQuestions = forwardQuestionRepository.findAll(spec, pageable);
        return forwardQuestions.map(this::mapToForwardQuestionDTO);
    }

    private ForwardQuestionDTO mapToForwardQuestionDTO(ForwardQuestionEntity forwardQuestion) {
        ForwardQuestionDTO.DepartmentDTO fromDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getFromDepartment().getId()).name(forwardQuestion.getFromDepartment().getName())
                .build();

        ForwardQuestionDTO.DepartmentDTO toDepartmentDTO = ForwardQuestionDTO.DepartmentDTO.builder()
                .id(forwardQuestion.getToDepartment().getId()).name(forwardQuestion.getToDepartment().getName())
                .build();

        ForwardQuestionDTO.ConsultantDTO consultantDTO = ForwardQuestionDTO.ConsultantDTO.builder()
                .id(forwardQuestion.getQuestion().getUser().getId())
                .firstName(forwardQuestion.getQuestion().getUser().getFirstName())
                .lastName(forwardQuestion.getQuestion().getUser().getLastName()).build();

        return ForwardQuestionDTO.builder().id(forwardQuestion.getId()).title(forwardQuestion.getTitle()).fromDepartment(fromDepartmentDTO)
                .toDepartment(toDepartmentDTO).consultant(consultantDTO)
                .statusForward(forwardQuestion.getStatusForward()).build();
    }
}
