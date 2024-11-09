package studentConsulting.service.implement.actor;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.AnswerEntity;
import studentConsulting.model.entity.QuestionEntity;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.actor.AnswerDTO;
import studentConsulting.model.payload.mapper.actor.AnswerMapper;
import studentConsulting.model.payload.request.CreateAnswerRequest;
import studentConsulting.model.payload.request.ReviewAnswerRequest;
import studentConsulting.model.payload.request.UpdateAnswerRequest;
import studentConsulting.repository.actor.AnswerRepository;
import studentConsulting.repository.actor.QuestionRepository;
import studentConsulting.repository.admin.RoleConsultantRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.implement.common.FileStorageServiceImpl;
import studentConsulting.service.interfaces.actor.IAnswerService;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class AnswerServiceImpl implements IAnswerService {

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
    private FileStorageServiceImpl fileStorageService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerMapper answerMapper;

    @Override
    public AnswerDTO createAnswer(CreateAnswerRequest request) {
        Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
        if (questionOpt.isEmpty()) {
            throw new ErrorException("Câu hỏi không tồn tại với ID: " + request.getQuestionId());
        }

        QuestionEntity question = questionOpt.get();

        if (Boolean.TRUE.equals(question.getStatusDelete())) {
            throw new ErrorException("Câu hỏi này đã bị xóa, không thể trả lời.");
        }

        boolean hasAnswers = answerRepository.existsByQuestionId(request.getQuestionId());
        if (hasAnswers) {
            throw new ErrorException("Câu hỏi này đã được trả lời, không thể trả lời lại.");
        }

        Optional<RoleConsultantEntity> roleConsultant = roleConsultantRepository.findById(request.getRoleConsultantId());
        if (roleConsultant.isEmpty()) {
            throw new ErrorException("Vai trò tư vấn không tồn tại.");
        }

        Optional<UserInformationEntity> user = userInformationRepository.findById(request.getConsultantId());
        if (user.isEmpty()) {
            throw new ErrorException("Người tư vấn không tồn tại với ID: " + request.getConsultantId());
        }

        if (!question.getDepartment().getId().equals(user.get().getAccount().getDepartment().getId())) {
            throw new ErrorException("Phòng ban của tư vấn viên không trùng với phòng ban của câu hỏi, không thể thực hiện trả lời.");
        }

        String fileName = null;
        if (request.getFile() != null && !request.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(request.getFile());
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
            return answerMapper.mapToAnswerDTO(savedAnswer);
        }

        return answerMapper.mapToAnswerDTO(savedAnswer);
    }

    @Override
    public AnswerDTO reviewAnswer(Integer questionId, ReviewAnswerRequest request) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(questionId);
        if (answerOpt.isEmpty()) {
            errors.add(new FieldErrorDetail("answerId", "Câu trả lời không tồn tại."));
            throw new CustomFieldErrorException(errors);
        }

        AnswerEntity answer = answerOpt.get();
        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            throw new ErrorException("Câu trả lời này đã được duyệt và không thể kiểm duyệt lại");
        }

        String fileName = null;
        if (request.getFile() != null && !request.getFile().isEmpty()) {
            fileName = fileStorageService.saveFile(request.getFile());
            answer.setFile(fileName);
        }

        answer.setContent(request.getContent());
        answer.setStatusAnswer(true);
        answer.setStatusApproval(false);

        Optional<QuestionEntity> questionOpt = questionRepository.findById(questionId);
        if (questionOpt.isEmpty()) {
            errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + questionId));
            throw new CustomFieldErrorException(errors);
        }

        QuestionEntity question = questionOpt.get();
        question.setStatusApproval(true);
        questionRepository.save(question);
        AnswerEntity reviewedAnswer = answerRepository.save(answer);

        return answerMapper.mapToAnswerDTO(reviewedAnswer);
    }


    @Override
    public AnswerDTO updateAnswer(Integer answerId, UpdateAnswerRequest request, UserInformationEntity user) {
        AnswerEntity existingAnswer;

        String userRole = user.getAccount().getRole().getName();

        if (userRole.equals(SecurityConstants.Role.ADMIN)) {
            existingAnswer = answerRepository.findById(answerId)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            existingAnswer = answerRepository.findByIdAndDepartmentId(answerId, user.getAccount().getDepartment().getId())
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));
        } else if (userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            existingAnswer = answerRepository.findById(answerId)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));

            if (!existingAnswer.getUser().getId().equals(user.getId())) {
                throw new ErrorException("Bạn không có quyền cập nhật câu trả lời này");
            }
        } else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        existingAnswer.setTitle(request.getTitle());
        existingAnswer.setContent(request.getContent());
        existingAnswer.setStatusApproval(request.getStatusApproval());
        existingAnswer.setStatusAnswer(request.getStatusAnswer());

        if (request.getFile() != null && !request.getFile().isEmpty()) {
            String fileName = request.getFile().getOriginalFilename();
            existingAnswer.setFile(fileName);
        }

        AnswerEntity updatedAnswer = answerRepository.save(existingAnswer);
        return answerMapper.mapToAnswerDTO(updatedAnswer);
    }

    @Override
    public void deleteAnswer(Integer id, UserInformationEntity user) {
        AnswerEntity existingAnswer;

        String userRole = user.getAccount().getRole().getName();
        System.out.println("a" + user.getAccount().getDepartment().getId());
        if (userRole.equals(SecurityConstants.Role.ADMIN)) {
            existingAnswer = answerRepository.findById(id)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            existingAnswer = answerRepository.findByIdAndDepartmentId(id, user.getAccount().getDepartment().getId())
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));
        } else if (userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            existingAnswer = answerRepository.findById(id)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));

            if (!existingAnswer.getUser().getId().equals(user.getId())) {
                throw new ErrorException("Bạn không có quyền xóa câu trả lời này");
            }
        } else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        existingAnswer.setTitle(null);
        existingAnswer.setContent(null);
        existingAnswer.setStatusApproval(null);
        existingAnswer.setStatusAnswer(null);
        existingAnswer.setFile(null);

        QuestionEntity relatedQuestion = existingAnswer.getQuestion();
        if (relatedQuestion != null) {
            relatedQuestion.setStatusApproval(false);
            questionRepository.save(relatedQuestion);
        }

        answerRepository.save(existingAnswer);
    }

    @Override
    public AnswerDTO getAnswerById(Integer answerId, UserInformationEntity user) {
        AnswerEntity answer;

        String userRole = user.getAccount().getRole().getName();

        if (userRole.equals(SecurityConstants.Role.ADMIN)) {
            answer = answerRepository.findById(answerId)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));
        } else if (userRole.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            answer = answerRepository.findByIdAndDepartmentId(answerId, user.getAccount().getDepartment().getId())
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại trong bộ phận của bạn"));
        } else if (userRole.equals(SecurityConstants.Role.TUVANVIEN)) {
            answer = answerRepository.findById(answerId)
                    .orElseThrow(() -> new ErrorException("Câu trả lời không tồn tại"));

            if (!answer.getUser().getId().equals(user.getId())) {
                throw new ErrorException("Bạn không có quyền xem chi tiết câu trả lời này");
            }
        } else {
            throw new ErrorException("Bạn không có quyền thực hiện hành động này");
        }

        return answerMapper.mapToAnswerDTO(answer);
    }
}