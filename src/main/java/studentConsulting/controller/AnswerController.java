package studentConsulting.controller;

import java.security.Principal;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;

@RestController
@RequestMapping("/api/v1/answer")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @PostMapping(value = "/create", consumes = { "multipart/form-data" })
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart("file") MultipartFile file,
            @RequestParam("statusApproval") Boolean statusApproval,
            Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            return ResponseEntity.badRequest().body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("User not found.")
                    .build()
            );
        }

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant(); 

        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder()
                .questionId(questionId)
                .title(title)
                .content(content)
                .file(file)
                .statusApproval(statusApproval)
                .roleConsultantId(roleConsultant.getId()) 
                .consultantId(user.getId()) 
                .build();

        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Answer created successfully.")
                .data(answerDTO)
                .build());
    }
    
    @PostMapping("/review")
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(
            @RequestBody CreateAnswerRequest reviewRequest, Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("User not found.")
                    .build()
            );
        }

        UserInformationEntity user = userOpt.get();

        if (!isConsultant(user)) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("You do not have permission to review this answer.")
                    .build()
            );
        }

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("No answer found for this question.")
                    .build()
            );
        }

        AnswerEntity answer = answerOpt.get();

        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("This answer has already been approved and cannot be reviewed again.")
                    .build()
            );
        }

        UserInformationEntity consultant = answer.getUser();

        // Kiểm tra xem trưởng ban có cùng department với tư vấn viên hay không
        if (!consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN).body(
                DataResponse.<AnswerDTO>builder()
                    .status("error")
                    .message("You do not have permission to review answers from another department.")
                    .build()
            );
        }

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
            .status("success")
            .message("Kiểm duyệt thành công")
            .data(reviewedAnswer)
            .build());
    }


    
    private boolean isConsultant(UserInformationEntity user) {
        return "TRUONGBANTUVAN".equals(user.getAccount().getRole().getName());
    }


}
