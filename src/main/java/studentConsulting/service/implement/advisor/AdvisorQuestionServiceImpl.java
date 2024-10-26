package studentConsulting.service.implement.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.department_field.FieldEntity;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.DeletionLogRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorQuestionService;
import studentConsulting.specification.question_answer.QuestionSpecification;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdvisorQuestionServiceImpl implements IAdvisorQuestionService {

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private DeletionLogRepository deletionLogRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

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
    public Page<DeletionLogEntity> getDeletionLogs(UserInformationEntity user, Pageable pageable) {
        Specification<DeletionLogEntity> spec = Specification.where(null);
        String userRole = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        switch (userRole) {
            case SecurityConstants.Role.ADMIN:
                break;

            case SecurityConstants.Role.TRUONGBANTUVAN:
                if (departmentId != null) {
                    spec = spec.and(QuestionSpecification.belongsToDepartment(departmentId));
                } else {
                    throw new ErrorException("Trưởng ban không thuộc phòng ban nào.");
                }
                break;

            case SecurityConstants.Role.TUVANVIEN:
                String deletedBy = user.getAccount().getEmail();
                spec = spec.and(QuestionSpecification.deletedByEmail(deletedBy));
                break;

            default:
                throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        return deletionLogRepository.findAll(spec, pageable);
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
                .filterStatus(questionFilterStatus.getDisplayName())
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
    public void importQuestions(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream()
                .skip(1)  // Skip header row
                .collect(Collectors.toList());

        List<MyQuestionDTO> questions = filteredData.stream()
                .map(row -> {
                    try {
                        Integer id = Integer.parseInt(row.get(0));
                        String title = row.get(1);
                        String content = row.get(2);
                        LocalDate createdAt = LocalDate.parse(row.get(3));
                        String status = row.get(4);
                        Integer departmentId = Integer.parseInt(row.get(5));
                        Integer fieldId = Integer.parseInt(row.get(6));
                        String askerFirstname = row.get(7);
                        String askerLastname = row.get(8);
                        Integer views = Integer.parseInt(row.get(9));

                        return MyQuestionDTO.builder()
                                .id(id)
                                .title(title)
                                .content(content)
                                .createdAt(createdAt)
                                .department(new MyQuestionDTO.DepartmentDTO(departmentId, null))
                                .field(new MyQuestionDTO.FieldDTO(fieldId, null))
                                .askerFirstname(askerFirstname)
                                .askerLastname(askerLastname)
                                .views(views)
                                .build();
                    } catch (Exception e) {
                        throw new ErrorException("Lỗi khi parse dữ liệu câu hỏi: " + e.getMessage());
                    }
                })
                .collect(Collectors.toList());

        questions.forEach(question -> {
            try {
                QuestionEntity entity = new QuestionEntity();
                entity.setId(question.getId());
                entity.setTitle(question.getTitle());
                entity.setContent(question.getContent());
                entity.setCreatedAt(question.getCreatedAt());
                entity.setViews(question.getViews());

                DepartmentEntity department = departmentRepository.findById(question.getDepartment().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy phòng ban với ID: " + question.getDepartment().getId()));
                FieldEntity field = fieldRepository.findById(question.getField().getId())
                        .orElseThrow(() -> new ErrorException("Không tìm thấy lĩnh vực với ID: " + question.getField().getId()));

                entity.setDepartment(department);
                entity.setField(field);

                questionRepository.save(entity);
            } catch (Exception e) {
                throw new ErrorException("Lỗi khi lưu câu hỏi vào database: " + e.getMessage());
            }
        });
    }
}
