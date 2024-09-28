package studentConsulting.service.implement.consultant;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RoleConsultantRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;
import studentConsulting.service.interfaces.consultant.IConsultantAnswerService;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class ConsultantAnswerServiceImpl implements IConsultantAnswerService {

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private UserRepository userInformationRepository;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @Override
    public AnswerDTO createAnswer(CreateAnswerRequest request) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
        if (questionOpt.isEmpty()) {
            errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + request.getQuestionId()));
        }
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        QuestionEntity question = questionOpt.get();

        boolean hasAnswers = answerRepository.existsByQuestionId(request.getQuestionId());

        if (hasAnswers) {
            errors.add(new FieldErrorDetail("questionId", "Câu hỏi này đã được trả lời, không thể trả lời lại."));
        }

        Optional<RoleConsultantEntity> roleConsultant = roleConsultantRepository.findById(request.getRoleConsultantId());
        if (roleConsultant.isEmpty()) {
            errors.add(new FieldErrorDetail("roleConsultant", "Vai trò tư vấn không tồn tại"));
        }

        Optional<UserInformationEntity> user = userInformationRepository.findById(request.getConsultantId());
        if (user.isEmpty()) {
            errors.add(new FieldErrorDetail("consultantId", "Người tư vấn không tồn tại với ID: " + request.getConsultantId()));
        }

        String fileName = null;
        if (request.getFile() != null && !request.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(request.getFile());
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        AnswerEntity answer = AnswerEntity.builder()
                .question(question)
                .roleConsultant(roleConsultant.get())
                .user(user.get())
                .title(request.getTitle())
                .content(request.getContent())
                .file(fileName)
                .statusApproval(request.getStatusApproval())
                .statusAnswer(true)
                .createdAt(LocalDate.now())
                .build();
        question.setStatusApproval(true);
        questionRepository.save(question);

        AnswerEntity savedAnswer = answerRepository.save(answer);

        if (request.getStatusApproval() != null && request.getStatusApproval()) {
            answer.setStatusAnswer(false);
            questionRepository.save(question);
            return mapToAnswerDTO(savedAnswer);
        }

        return mapToAnswerDTO(savedAnswer);
    }

    public AnswerDTO mapToAnswerDTO(AnswerEntity answer) {
        return AnswerDTO.builder()
                .answerId(answer.getId())
                .questionId(answer.getQuestion().getId())
                .roleConsultantId(answer.getRoleConsultant().getId())
                .userId(answer.getUser().getId())
                .title(answer.getTitle())
                .content(answer.getContent())
                .file(answer.getFile())
                .createdAt(answer.getCreatedAt())
                .statusApproval(answer.getStatusApproval())
                .statusAnswer(answer.getStatusAnswer())
                .build();
    }
}