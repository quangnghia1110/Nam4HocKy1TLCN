package studentConsulting.model.payload.mapper.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.question_answer.AnswerEntity;
import studentConsulting.model.entity.question_answer.QuestionEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.dto.question_answer.QuestionDTO;
import studentConsulting.model.payload.request.question_answer.CreateFollowUpQuestionRequest;
import studentConsulting.model.payload.request.question_answer.CreateQuestionRequest;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.department_field.FieldRepository;
import studentConsulting.repository.question_answer.AnswerRepository;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.RoleAskRepository;
import studentConsulting.repository.user.UserRepository;

import java.time.LocalDate;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

@Component
public class QuestionMapper {

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private FieldRepository fieldRepository;

    @Autowired
    private RoleAskRepository roleAskRepository;

    public MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question) {
        String askerFirstname = question.getUser().getFirstName();
        String askerLastname = question.getUser().getLastName();
        String askerAvatarUrl = question.getUser().getAvatarUrl();

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

        // Thiết lập trạng thái của câu hỏi
        QuestionFilterStatus questionFilterStatus;
        if (Boolean.TRUE.equals(question.getStatusPublic()) && (question.getStatusDelete() == null)) {
            questionFilterStatus = QuestionFilterStatus.PUBLIC;
        } else if (Boolean.TRUE.equals(question.getStatusDelete()) && Boolean.TRUE.equals(question.getStatusPublic())) {
            questionFilterStatus = QuestionFilterStatus.DELETED;
        } else if (Boolean.TRUE.equals(question.getStatusDelete()) && Boolean.FALSE.equals(question.getStatusPublic())) {
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
                .askerFirstname(askerFirstname)
                .askerLastname(askerLastname)
                .askerAvatarUrl(askerAvatarUrl)
                .department(departmentDTO)
                .field(fieldDTO)
                .roleAsk(roleAskDTO)
                .filterStatus(questionFilterStatus.getDisplayName())
                .build();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(question.getId());
        answerOpt.ifPresent(answer -> {
            if (answer.getTitle() == null && answer.getContent() == null) {
                dto.setAnswerId(null);
                dto.setAnswerTitle(null);
                dto.setAnswerContent(null);
                dto.setAnswerUserEmail(null);
                dto.setAnswerUserFirstname(null);
                dto.setAnswerUserLastname(null);
                dto.setAnswerCreatedAt(null);
                dto.setAnswerAvatarUrl(null);
            } else {
                dto.setAnswerId(answer.getId());
                dto.setAnswerTitle(Optional.ofNullable(answer.getTitle()).orElse(null));
                dto.setAnswerContent(Optional.ofNullable(answer.getContent()).orElse(null));
                dto.setAnswerUserEmail(Optional.ofNullable(answer.getUser())
                        .map(user -> user.getAccount().getEmail())
                        .orElse(null));
                dto.setAnswerUserFirstname(Optional.ofNullable(answer.getUser())
                        .map(user -> user.getFirstName())
                        .orElse(null));
                dto.setAnswerUserLastname(Optional.ofNullable(answer.getUser())
                        .map(user -> user.getLastName())
                        .orElse(null));
                dto.setAnswerCreatedAt(Optional.ofNullable(answer.getCreatedAt()).orElse(null));
                dto.setAnswerAvatarUrl(Optional.ofNullable(answer.getUser())
                        .map(user -> user.getAvatarUrl())
                        .orElse(null));
            }
        });

        return dto;
    }

    public MyQuestionDTO mapToMyQuestionDTO(QuestionEntity question, Set<Integer> processedQuestionIds) {
        if (processedQuestionIds.contains(question.getId())) {
            return null;
        }

        processedQuestionIds.add(question.getId());

        MyQuestionDTO dto = mapToMyQuestionDTO(question);

        List<MyQuestionDTO> followUpQuestions = questionRepository.findFollowUpQuestionsByParentId(question.getId()).stream()
                .map(followUpQuestion -> mapToMyQuestionDTO(followUpQuestion, processedQuestionIds))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        dto.setFollowUpQuestions(followUpQuestions);

        return dto;
    }


    public QuestionEntity mapDTOToEntity(QuestionDTO questionDTO, Integer userId) {
        QuestionEntity question = new QuestionEntity();

        question.setTitle(questionDTO.getTitle());
        question.setContent(questionDTO.getContent());
        question.setStatusPublic(questionDTO.getStatusPublic());
        question.setViews(questionDTO.getViews());

        UserInformationEntity user = userRepository.findById(userId)
                .orElseThrow(() -> new RuntimeException("User not found"));
        user.setFirstName(questionDTO.getFirstName());
        user.setLastName(questionDTO.getLastName());
        question.setUser(user);

        question.setDepartment(
                departmentRepository.findById(questionDTO.getDepartmentId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Phòng ban không tồn tại với id: " + questionDTO.getDepartmentId()))
        );

        question.setField(
                fieldRepository.findById(questionDTO.getFieldId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Lĩnh vực không tồn tại với id: " + questionDTO.getFieldId()))
        );

        question.setRoleAsk(
                roleAskRepository.findById(questionDTO.getRoleAskId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Vai trò không tồn tại với id: " + questionDTO.getRoleAskId()))
        );

        question.setFileName(questionDTO.getFileName());
        question.setCreatedAt(LocalDate.now());

        return question;
    }

    public QuestionDTO mapEntityToDTO(QuestionEntity question) {
        return QuestionDTO.builder()
                .id(question.getId()).departmentId(question.getDepartment().getId()).fieldId(question.getField().getId())
                .roleAskId(question.getRoleAsk().getId()).title(question.getTitle()).content(question.getContent())
                .firstName(question.getUser().getFirstName()).lastName(question.getUser().getLastName())
                .statusPublic(question.getStatusPublic()).fileName(question.getFileName())
                .statusApproval(question.getStatusApproval()).build();
    }

    public QuestionDTO mapRequestToDTO(CreateQuestionRequest request, String fileName) {
        return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
                .roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
                .firstName(request.getFirstName()).lastName(request.getLastName())
                .statusPublic(request.getStatusPublic()).fileName(fileName).statusApproval(false).build();
    }

    public QuestionDTO mapRequestToDTO(CreateFollowUpQuestionRequest request, String fileName) {
        return QuestionDTO.builder().departmentId(request.getDepartmentId()).fieldId(request.getFieldId())
                .roleAskId(request.getRoleAskId()).title(request.getTitle()).content(request.getContent())
                .firstName(request.getFirstName()).lastName(request.getLastName()).studentCode(request.getStudentCode())
                .statusPublic(request.getStatusPublic()).fileName(fileName).build();
    }
}
