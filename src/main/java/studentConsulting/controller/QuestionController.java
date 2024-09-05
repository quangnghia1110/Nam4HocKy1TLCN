package studentConsulting.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.payload.dto.QuestionDTO;
import studentConsulting.model.payload.dto.RoleAskDTO;
import studentConsulting.model.payload.request.question.CreateQuestionRequest;
import studentConsulting.model.payload.request.question.UpdateQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.IQuestionService;

@RestController
@RequestMapping("/api/v1/question")
public class QuestionController {
    
    @Autowired
    private IQuestionService questionService;
    
    @PostMapping(value = "/create", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<QuestionDTO>> createQuestion(
            @RequestParam("departmentId") Integer departmentId,
            @RequestParam("fieldId") Integer fieldId,
            @RequestParam("roleAskId") Integer roleAskId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("firstName") String firstName,
            @RequestParam("lastName") String lastName,
            @RequestParam("studentCode") String studentCode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestPart("file") MultipartFile file) {
        
        CreateQuestionRequest questionRequest = CreateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .studentCode(studentCode)
                .statusPublic(statusPublic)
                .file(file)
                .build();
        
        return ResponseEntity.ok(questionService.createQuestion(questionRequest));
    }
    
    @PostMapping(value = "/update", consumes = {"multipart/form-data"})
    public ResponseEntity<DataResponse<QuestionDTO>> updateQuestion(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("departmentId") Integer departmentId,
            @RequestParam("fieldId") Integer fieldId,
            @RequestParam("roleAskId") Integer roleAskId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("firstName") String firstName,
            @RequestParam("lastName") String lastName,
            @RequestParam("studentCode") String studentCode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestPart(value = "file", required = false) MultipartFile file) {

        UpdateQuestionRequest questionRequest = UpdateQuestionRequest.builder()
                .departmentId(departmentId)
                .fieldId(fieldId)
                .roleAskId(roleAskId)
                .title(title)
                .content(content)
                .firstName(firstName)
                .lastName(lastName)
                .studentCode(studentCode)
                .statusPublic(statusPublic)
                .file(file)
                .build();

        return ResponseEntity.ok(questionService.updateQuestion(questionId, questionRequest));
    }
    
    @DeleteMapping("/delete/{id}")
    public ResponseEntity<DataResponse<Void>> deleteQuestion(@PathVariable("id") Integer questionId) {
        return ResponseEntity.ok(questionService.deleteQuestion(questionId));
    }
    
    @GetMapping("/roleAsk")
    public ResponseEntity<DataResponse<List<RoleAskDTO>>> getAllRoleAsk() {
        List<RoleAskDTO> roleAsks = questionService.getAllRoleAsk();
        DataResponse<List<RoleAskDTO>> response = DataResponse.<List<RoleAskDTO>>builder()
                .status("success")
                .message("Fetched all role ask successfully.")
                .data(roleAsks)
                .build();

        return ResponseEntity.ok(response);
    }
}

