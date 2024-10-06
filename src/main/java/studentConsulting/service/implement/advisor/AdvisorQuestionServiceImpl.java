package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.DeletionLogRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorQuestionService;
import studentConsulting.specification.question_answer.QuestionSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdvisorQuestionServiceImpl implements IAdvisorQuestionService {

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DeletionLogRepository deletionLogRepository;

    @Override
    public Page<MyQuestionDTO> getDepartmentConsultantsQuestionsFilters(Integer departmentId, String title, String status, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<QuestionEntity> spec = Specification.where(QuestionSpecification.hasConsultantsInDepartment(departmentId));

        if (title != null && !title.isEmpty()) {
            spec = spec.and(QuestionSpecification.hasTitle(title));
        }

        if (status != null && !status.isEmpty()) {
            QuestionFilterStatus filterStatus = QuestionFilterStatus.fromKey(status);
            spec = spec.and(QuestionSpecification.hasStatus(filterStatus));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(QuestionSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(QuestionSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(QuestionSpecification.hasDateBefore(endDate));
        }

        Page<QuestionEntity> questionEntities = questionRepository.findAll(spec, pageable);
        return questionEntities.map(this::mapToMyQuestionDTO);
    }

    @Override
    public Page<DeletionLogEntity> getDeletionLogsByDepartment(Integer departmentId, Pageable pageable) {
        return deletionLogRepository.findAllByDepartmentId(departmentId, pageable);
    }

    private MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question) {
        MyQuestionDTO.DepartmentDTO departmentDTO = MyQuestionDTO.DepartmentDTO.builder()
                .id(question.getDepartment().getId())
                .name(question.getDepartment().getName())
                .build();

        MyQuestionDTO.FieldDTO fieldDTO = MyQuestionDTO.FieldDTO.builder()
                .id(question.getField().getId())
                .name(question.getField().getName())
                .build();

        MyQuestionDTO.RoleAskDTO roleAskDTO = MyQuestionDTO.RoleAskDTO.builder()
                .id(question.getRoleAsk().getId())
                .name(question.getRoleAsk().getName())
                .build();

        QuestionFilterStatus questionFilterStatus;
        if (Boolean.TRUE.equals(question.getStatusPublic()) && (question.getStatusDelete() == null)) {
            questionFilterStatus = QuestionFilterStatus.PUBLIC;
        } else if (Boolean.TRUE.equals(question.getStatusDelete()) && Boolean.TRUE.equals(question.getStatusPublic())) {
            questionFilterStatus = QuestionFilterStatus.DELETED;
        } else if (Boolean.FALSE.equals(question.getStatusPublic()) && (question.getStatusDelete() == null)) {
            questionFilterStatus = QuestionFilterStatus.PRIVATE;
        } else {
            questionFilterStatus = QuestionFilterStatus.NOT_ANSWERED;
        }

        MyQuestionDTO dto = MyQuestionDTO.builder()
                .id(question.getId())
                .title(question.getTitle())
                .content(question.getContent())
                .createdAt(question.getCreatedAt())
                .views(question.getViews())
                .fileName(question.getFileName())
                .department(departmentDTO)
                .field(fieldDTO)
                .roleAsk(roleAskDTO)
                .questionFilterStatus(questionFilterStatus)
                .build();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(question.getId());
        answerOpt.ifPresent(answer -> {
            dto.setAnswerTitle(answer.getTitle());
            dto.setAnswerContent(answer.getContent());
            dto.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
            dto.setAnswerUserFirstname(answer.getUser().getFirstName());
            dto.setAnswerUserLastname(answer.getUser().getLastName());
            dto.setAnswerCreatedAt(answer.getCreatedAt());
            dto.setAnswerAvatarUrl(answer.getUser().getAvatarUrl());
        });

        return dto;
    }

    @Override
    public MyQuestionDTO getQuestionByIdAndDepartment(Integer questionId, Integer departmentId) {
        Optional<QuestionEntity> questionOpt = questionRepository.findByIdAndDepartmentId(questionId, departmentId);
        if (!questionOpt.isPresent()) {
            throw new ErrorException("Câu hỏi không tồn tại");
        }
        QuestionEntity question = questionOpt.get();
        return mapToMyQuestionDTO(question);
    }
}
