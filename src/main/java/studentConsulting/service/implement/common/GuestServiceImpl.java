package studentConsulting.service.implement.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.CommonQuestionEntity;
import studentConsulting.model.entity.FieldEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.payload.dto.actor.*;
import studentConsulting.model.payload.mapper.actor.CommonQuestionMapper;
import studentConsulting.model.payload.mapper.actor.QuestionMapper;
import studentConsulting.model.payload.mapper.admin.UserInformationMapper;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.CommonQuestionRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.*;
import studentConsulting.service.interfaces.common.IGuestService;
import studentConsulting.specification.actor.CommonQuestionSpecification;
import studentConsulting.specification.actor.QuestionSpecification;
import studentConsulting.specification.common.ConsultantSpecification;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class GuestServiceImpl implements IGuestService {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    @Autowired
    private CommonQuestionMapper commonQuestionMapper;

    @Autowired
    private QuestionMapper questionMapper;

    @Autowired
    private UserInformationMapper userMapper;

    @Autowired
    private AnswerRepository answerRepository;

    @Override
    public Page<ConsultantDTO> getConsultant(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<UserInformationEntity> spec = Specification.where(ConsultantSpecification.hasRole(SecurityConstants.Role.TUVANVIEN));

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

        return userRepository.findAll(spec, pageable).map(userMapper::mapDTO);
    }

    @Override
    public List<UserDTO> getConsultantByDepartment(Integer departmentId) {
        List<UserInformationEntity> consultants = userRepository.findAll().stream()
                .filter(user -> user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) &&
                        user.getAccount().getDepartment().getId().equals(departmentId))
                .collect(Collectors.toList());

        return consultants.stream()
                .map(consultant -> new UserDTO(consultant.getId(), consultant.getFirstName(), consultant.getLastName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<UserDTO> getConsultantTeacherByDepartment(Integer departmentId) {
        List<UserInformationEntity> consultants = userRepository.findAll().stream()
                .filter(user -> user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) &&
                        user.getAccount().getRoleConsultant().getName().equals(SecurityConstants.RoleConsultant.GIANGVIEN) &&
                        user.getAccount().getDepartment().getId().equals(departmentId))
                .collect(Collectors.toList());

        return consultants.stream()
                .map(consultant -> new UserDTO(consultant.getId(), consultant.getFirstName(), consultant.getLastName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<UserDTO> getConsultantStudentByDepartment(Integer departmentId) {
        List<UserInformationEntity> consultants = userRepository.findAll().stream()
                .filter(user -> user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN) &&
                        user.getAccount().getRoleConsultant().getName().equals(SecurityConstants.RoleConsultant.SINHVIEN) &&
                        user.getAccount().getDepartment().getId().equals(departmentId))
                .collect(Collectors.toList());

        return consultants.stream()
                .map(consultant -> new UserDTO(consultant.getId(), consultant.getFirstName(), consultant.getLastName()))
                .collect(Collectors.toList());
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestion(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<CommonQuestionEntity> spec = Specification.where(CommonQuestionSpecification.hasStatusTrue());

        if (departmentId != null) {
            spec = spec.and(CommonQuestionSpecification.isCreatedByAdvisor(departmentId));
        }

        if (title != null && !title.isEmpty()) {
            spec = spec.and(CommonQuestionSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(CommonQuestionSpecification.hasDateBefore(endDate));
        }

        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(spec, pageable);
        return commonQuestions.map(commonQuestionMapper::mapToDTO);
    }

    @Override
    public List<DepartmentDTO> getAllDepartment() {
        return departmentRepository.findAll().stream()
                .map(department -> new DepartmentDTO(department.getId(), department.getName()))
                .collect(Collectors.toList());
    }

    @Override
    public List<FieldDTO> getFieldByDepartment(Integer departmentId) {
        List<FieldEntity> fields = fieldRepository.findAll().stream()
                .filter(field -> field.getDepartment().getId().equals(departmentId))
                .collect(Collectors.toList());

        return fields.stream()
                .map(field -> new FieldDTO(field.getId(), field.getName()))
                .collect(Collectors.toList());
    }

    @Override
    public Page<MyQuestionDTO> getQuestion(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.isPublicAndAnswered())
                .and(QuestionSpecification.hasApprovedStatus())
                .and(QuestionSpecification.hasStatusFalse());

        if (departmentId != null) {
            spec = spec.and(QuestionSpecification.hasConsultantsInDepartment(departmentId));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questions = questionRepository.findAll(spec, pageable);
        return questions.map(question -> questionMapper.mapToMyQuestionDTOs(question, answerRepository));
    }


    @Override
    public List<RoleAskDTO> getAllRoleAsk() {
        return roleAskRepository.findAll().stream().map(roleAsk -> new RoleAskDTO(roleAsk.getId(), roleAsk.getName()))
                .collect(Collectors.toList());
    }
}
